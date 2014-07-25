-- module SLParse (parseString, parseFile, Error, parse, parseFun) where
module SLParse where

import Data.Char
import Data.Int
import Data.Either
import Control.Applicative hiding ((<|>), Const, many)
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Prim
import SuperLambda

type Error = ParseError

parseString :: String -> Either Error Program
parseString = parse parseProgram ""

parseFile :: FilePath -> IO (Either Error Program)
parseFile file =
    do  input <- readFile file
        return $ parseString input

parseProgram :: Parser Program
parseProgram = do fs <- many parseFun
                  symbol "##"
                  expr <- parseExpr <* eof
                  return $ Let (map trans fs) expr
    where
        trans :: Fun -> (Name, Expr)
        trans (FunDef id args body) = (id, Lambda args body)

parseFun :: Parser Fun
parseFun = do
  id <- parseVar
  args <- parens $ commaSep1 parseVar
  symbol "="
  expr <- parseExpr
  return $ FunDef id args expr

parseDecl :: Parser (Name, Expr)
parseDecl = do
  id <- parseVar
  spaces
  symbol "="
  expr <- parseExpr
  return $ (id, expr)


parseExpr :: Parser Expr
parseExpr = do
  e <- parseExpr1
  next <- (Just <$> choice (map (try . string) ["<", ">", "==", "<=", ">=", "=<", "=>", "!="]) <* spaces) <|> return Nothing
  case next of
    Just "<"  -> BinOp Gt  <$> parseExpr <*> return e
    Just ">"  -> BinOp Gt  <$> return e  <*> parseExpr
    Just "<=" -> BinOp Gte <$> parseExpr <*> return e
    Just "=<" -> BinOp Gte <$> parseExpr <*> return e
    Just ">=" -> BinOp Gte <$> return e  <*> parseExpr
    Just "=>" -> BinOp Gte <$> return e  <*> parseExpr
    Just "==" -> BinOp Eq  <$> return e  <*> parseExpr
    Just "!=" -> do
      e1 <- parseExpr
      return $ BinOp Eq (IntVal 0) (BinOp Eq e e1)
    Nothing  -> return e

parseExpr1 :: Parser Expr
parseExpr1 = do
  e <- parseExpr2
  next <- (Just <$> oneOf "+-" <* spaces)  <|> return Nothing
  case next of
    Just '+' -> BinOp Add e <$> parseExpr1
    Just '-' -> BinOp Sub e <$> parseExpr1
    Nothing  -> return e

parseExpr2 :: Parser Expr
parseExpr2 = do
  e <- parseExpr3
  next <- (Just <$> oneOf "*/" <* spaces)  <|> return Nothing
  case next of
    Just '*' -> BinOp Mul e <$> parseExpr2
    Just '/' -> BinOp Div e <$> parseExpr2
    Nothing  -> return e

parseExpr3 :: Parser Expr
parseExpr3 = do
  next <- try (Just <$> choice (map string keywords) <* spaces) <|> return Nothing
  case next of
    Nothing -> parseExpr4
    Just "if" -> do
      e1 <- parseExpr
      symbol "then"
      e2 <- parseExpr
      symbol "else"
      e3 <- parseExpr
      return $ IfThenElse e1 e2 e3
    Just "let" -> do
      decls <- many1 parseDecl
      symbol "in"
      expr <- parseExpr
      return $ Let decls expr
    Just "fst"   -> UnOp Car    <$> parseExpr3
    Just "snd"   -> UnOp Cdr    <$> parseExpr3
    Just "atom"  -> UnOp Atom   <$> parseExpr3
    Just "print" -> BinOp Print <$> parseExpr <*> (symbol ";" >> parseExpr)
    Just "break" -> UnOp Break  <$> (symbol ";" >> parseExpr)


keywords = ["if", "let", "fst", "snd", "atom", "print", "break"]

parseExpr4 :: Parser Expr
parseExpr4 = do
  next <- try ((char '\\' >> return True) <|> return False)
  if next
    then do args <- parseVar `endBy` (symbol " ")
            symbol "->"
            expr <- parseExpr
            return $ Lambda args expr
    else parseExpr5

parseExpr5 :: Parser Expr
parseExpr5 = do
  e <- parseExpr6
  next <- lookAhead ((char '(' >> return True) <|> return False)
  if next
    then CallFun e <$> parseExprs
    else return e

parseExpr6 :: Parser Expr
parseExpr6 = (IntVal <$> parseVal) <|> (Var <$> parseVar) <|> parseTuple

parseVar :: Parser String
parseVar = do
  c <- (letter <|> char '_')
  cs <- many (letter <|> digit <|> char '_')
  spaces
  return (c : cs)

parseVal :: Parser Int32
parseVal = liftA read $ many1 digit <* spaces

parseTuple :: Parser Expr
parseTuple = do
  exprs <- parseExprs
  return (foldr1 (BinOp Cons) exprs)

parseExprs :: Parser [Expr]
parseExprs = parens (commaSep1 parseExpr) <* spaces

-- Helper functions

commaSep p  = p `sepBy` (symbol ",")
commaSep1 p = p `sepBy1` (symbol ",")

parens = between (symbol "(") (symbol ")")

symbol :: String -> Parser ()
symbol s = do string s
              spaces
              return ()


word :: Parser String
word = many1 letter
