module Parser where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import Util
import Lc0

parseProg :: SourceName -> String -> Result Lc0Expr
parseProg srcname src
  = Trace "Parsing..." . to_result
  $ parse (optional ws *> expr <* eof) srcname src
  where
    to_result (Left err)  = Util.Error (print err)
    to_result (Right res) = Util.Success res

expr :: Parser Lc0Expr
expr = mkApChain Ap <$> many exprNAp

exprNAp :: Parser Lc0Expr
exprNAp = (str "(" *> expr <* str ")")
      <|> (Val <$> lambda)
      <|> (Var <$> symbol)

lambda :: Parser Lc0Lambda
lambda = str "(" *> lambda <* str ")"
     <|> str "\\" *> lambdaBody

lambdaBody :: Parser Lc0Lambda
lambdaBody = _arg <|> _expr
  where
    _arg = Arg <$> symbol <*> lambdaBody
    _expr = Expr <$> expr

ws :: Parser ()
ws = void $ _ws *> optional (str "--" *> manyTill anyChar (str "\n" <|> str "--") *> _ws)
  where _ws = many $ oneOf "\n\t "

symbol :: Parser String
symbol = many1 (noneOf "\\()\n\t ") <* optional ws

str :: String -> Parser String
str s = string s <* optional ws

-- TODO this is very ineficient
mkApChain :: (a -> a -> a) -> [a] -> a
mkApChain _ []   = error "Empty expression"
mkApChain _ [x]  = x
mkApChain f xs = f (mkApChain f $ init xs) (last xs)
