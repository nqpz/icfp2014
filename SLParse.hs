module SLParse (parseString, parseFile, Error) where

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
                  symbol "--"
                  expr <- parseExpr <* eof
                  return $ Let (map trans fs) expr
    where
        trans :: Fun -> (Name, Expr)
        trans (FunDef id args body) = (id, Lambda args body)

parseExpr :: Parser Expr
parseExpr = exp1
    where
        exp1 = ((try call) <|> ifte <|> letv <|> lambda <|> exp2) <* spaces
        exp2 =(try val) <|> (try varr) <|> unop
        ifte = do symbol "if"
                  e1 <- parseExpr
                  symbol "then"
                  e2 <- parseExpr
                  symbol "else"
                  e3 <- parseExpr
                  return $ IfThenElse e1 e2 e3

        decl = do id <- var
                  spaces
                  symbol "="
                  expr <- parseExpr
                  return $ (id, expr)

        letv = do symbol "let"
                  decls <- many1 decl
                  symbol "in"
                  expr <- exp1
                  return $ Let decls expr

        call = do id <- exp2
                  args <- parens $ commaSep parseExpr
                  return $ CallFun id args

        varr = do id <- var
                  return $ Var id

        val  = do num <- parseInt32
                  return $ IntVal num

        lambda = parens $ innerLambda

        unop = car <|> cdr <|> atom
        car = do symbol "fst"
                 expr <- exp1
                 return $ UnOp Car expr
        cdr = do symbol "snd"
                 expr <- exp1
                 return $ UnOp Cdr expr
        atom = do symbol "atom"
                  expr <- exp1
                  return $ UnOp Atom expr

        innerLambda = do symbol "\\"
                         args <- var `endBy1` (symbol " ")
                         symbol "->"
                         expr <- parseExpr
                         return $ Lambda args expr



parseFun :: Parser Fun
parseFun = do id <- var
              args <- parens $ commaSep1 var
              spaces
              symbol "="
              expr <- parseExpr
              return $ FunDef id args expr


-- Helper functions

commaSep p  = p `sepBy` (symbol ",")
commaSep1 p = p `sepBy1` (symbol ",")

symbol :: String -> Parser ()
symbol s = do string s
              spaces
              return ()

keywords = ["let", "in", "if", "then", "else", "fst", "snd", "atom"]

parseInt :: Parser Int
parseInt = liftA read $ many1 digit <* spaces

parseInt32 :: Parser Int32
parseInt32 = liftA read $ many1 digit <* spaces

word :: Parser String
word = many1 letter

var :: Parser String
var = do notFollowedBy $ choice $ map string keywords
         many1 (letter <|> digit <|> char '_')

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")
