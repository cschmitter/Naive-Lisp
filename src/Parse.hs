-- |
module Parse where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

data Expr
  = Name String
  | Func String [Expr]
  | Let String Expr
  | Def String [String] Expr
  | Do [Expr]
  | If Expr Expr Expr
  | Paren Expr
  | L Literal
  deriving (Show, Eq)

data Literal
  = Int Int
  | Double Double
  | Char Char
  | String String
  | List [Literal]
  deriving (Show, Eq)

testParse :: IO ()
testParse = do
  line <- getLine
  case line of
    ":q" -> return ()
    _ -> do
      print $ myParse pExpr line
      testParse

myParse :: Parser a -> String -> Either ParseError a
myParse p = parse p ""

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = whitespace *> p <* whitespace

pExpr :: Parser Expr
pExpr =
  lexeme $
  pParen <|> (L <$> pLiteral) <|> try pLet <|> try pDef <|> try pDo <|>
  try pIf <|>
  try pFunc <|>
  pName

pIf :: Parser Expr
pIf = do
  _ <- lexeme $ string "if"
  condition <- lexeme $ try pFunc <|> (L <$> pLiteral) <|> pParen <|> pName
  true <- pExpr
  _ <- lexeme $ string "else"
  false <- pExpr
  return $ If condition true false

pDo :: Parser Expr
pDo = do
  _ <- lexeme $ string "do"
  exprs <- many1 pExpr
  return $ Do exprs

pParen :: Parser Expr
pParen = do
  _ <- char '('
  p <- pExpr
  _ <- char ')'
  return $ Paren p

pName :: Parser Expr
pName = Name <$> (many1 . oneOf $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z'])

pFunc :: Parser Expr
pFunc = do
  f_name <- many1 . oneOf $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
  _ <- char '('
  args <- many . lexeme $ try pFunc <|> (L <$> pLiteral) <|> pParen <|> pName
  _ <- char ')'
  return $ Func f_name args

pLet :: Parser Expr
pLet = do
  _ <- string "let"
  whitespace
  Name n <- pName
  whitespace
  Let n <$> pExpr

pDef :: Parser Expr
pDef = do
  _ <- lexeme $ string "def"
  Name n <- lexeme pName
  _ <- char '('
  params <- many . lexeme $ (\(Name a) -> a) <$> pName
  _ <- char ')'
  Def n params <$> lexeme pParen

pLiteral :: Parser Literal
pLiteral = lexeme $ try pDouble <|> pInt <|> pChar <|> pString <|> pList

pInt :: Parser Literal
pInt = do
  s <- option '0' $ char '-'
  n <- many1 digit
  return . Int $ read $ s : n

pDouble :: Parser Literal
pDouble = do
  Int x <- pInt
  dot <- char '.'
  y <- many1 digit
  let value = show x ++ dot : y
  return . Double $ read value

pChar :: Parser Literal
pChar = do
  _ <- char '\''
  c <- anyChar
  _ <- char '\''
  return $ Char c

pString :: Parser Literal
pString = do
  _ <- char '"'
  s <- manyTill anyChar (try $ char '"')
  return $ String s

pList :: Parser Literal
pList = do
  _ <- lexeme $ char '['
  xs <- try (many1 pLiteral) <|> return []
  _ <- lexeme $ char ']'
  return $ List xs
