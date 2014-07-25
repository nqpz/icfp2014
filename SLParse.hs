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

parse' :: Parser a -> String -> Either Error a
parse' p s = parse p "" s

parseString :: String -> Either Error Program
parseString = parse parseProgram ""

parseFile :: FilePath -> IO (Either Error Program)
parseFile file =
    do  input <- readFile file
        return $ parseString input

parseProgram :: Parser Program
parseProgram = do fs <- many parseFun
                  symbol "##"
                  expr <- parseStmt <* eof
                  return $ Let (map trans fs) expr
    where
        trans :: Fun -> (Name, Expr)
        trans (FunDef id args body) = (id, Lambda args body)

parseFun :: Parser Fun
parseFun = do
  id <- parseVar
  args <- parens $ commaSep1 parseVar
  symbol "="
  expr <- parseStmt
  return $ FunDef id args expr

parseDecl :: Parser (Name, Expr)
parseDecl = do
  id <- parseVar
  symbol "="
  expr <- parseStmt
  return $ (id, expr)

parseStmt :: Parser Expr
parseStmt = do
  key <- eatKeyword ["print", "break"]
  case key of
    Nothing -> parseStmt1
    Just "print" -> do
      e <- parseExpr
      symbol ";"
      Seq (Print e) <$> parseStmt
    Just "break" -> do
      symbol ";"
      Seq Break <$> parseStmt

parseStmt1 :: Parser Expr
parseStmt1 = do
  e <- parseExpr
  case e of
    Var n -> do
      assign <- (char '=' >> spaces >> return True) <|> return False
      case assign of
        False -> return e
        True  -> do
          e1 <- parseExpr
          symbol ";"
          Seq (Store n e1) <$> parseStmt
    _ -> return e

parseExpr :: Parser Expr
parseExpr = do
  e <- parseExpr1
  next <- choice' ["==", "<=", ">=", "=<", "=>", "!=", ">", "<"]
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
  next <- try (Just <$> (oneOf "*/" <* spaces)) <|> return Nothing
  case next of
    Just '*' -> BinOp Mul e <$> parseExpr2
    Just '/' -> BinOp Div e <$> parseExpr2
    Nothing  -> return e

parseExpr3 :: Parser Expr
parseExpr3 = do
  keyword <- eatKeyword ["if", "let", "fst", "snd", "atom"]
  case keyword of
    Nothing -> parseExpr4
    Just "if" -> do
      e1 <- parseStmt
      symbol "then"
      e2 <- parseStmt
      symbol "else"
      e3 <- parseStmt
      return $ IfThenElse e1 e2 e3
    Just "let" -> do
      decls <- many1 parseDecl
      symbol "in"
      expr <- parseStmt
      return $ Let decls expr
    Just "fst"   -> UnOp Car    <$> parseExpr3
    Just "snd"   -> UnOp Cdr    <$> parseExpr3
    Just "atom"  -> UnOp Atom   <$> parseExpr3

parseExpr4 :: Parser Expr
parseExpr4 = do
  next <- try ((char '\\' >> return True) <|> return False)
  if next
    then do args <- parseVar `endBy` (symbol " ")
            symbol "->"
            expr <- parseStmt
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
parseExprs = parens (commaSep1 parseStmt) <* spaces

-- Helper functions

eatKeyword :: [String] -> Parser (Maybe String)
eatKeyword keywords = do
  s <- lookAhead (Just <$> many1 letter) <|> return Nothing
  case s of
    Nothing -> return Nothing
    Just s  -> if s `elem` keywords
               then many1 letter >> spaces >> return (Just s)
               else return Nothing

choice' :: [String] -> Parser (Maybe String)
choice' s = (Just <$> choice (map symbol s)) <|> return Nothing

commaSep p  = p `sepBy` (symbol ",")
commaSep1 p = p `sepBy1` (symbol ",")

parens = between (symbol "(") (symbol ")")

symbol :: String -> Parser String
symbol s = do try (string s)
              spaces
              return s


word :: Parser String
word = many1 letter
