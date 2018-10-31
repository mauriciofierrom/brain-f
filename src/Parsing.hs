{-# LANGUAGE OverloadedStrings #-}

module Parsing ( program ) where

import Lib
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Sequence as S (fromList)

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

plus :: Parser Expr
plus = symbol "+" >> return (Command IncByte)

minus :: Parser Expr
minus = symbol "-" >> return (Command DecByte)

dot :: Parser Expr
dot = symbol "." >> return (Command PrintByte)

comma :: Parser Expr
comma = symbol "," >> return (Command GetByte)

lg :: Parser Expr
lg = symbol "<" >> return (Command DecPointer)

gt :: Parser Expr
gt = symbol ">" >> return (Command IncPointer)

loop :: Parser Expr
loop = do
  commandList <- brackets (some command)
  return $ Loop (S.fromList commandList)

command :: Parser Expr
command = plus
        <|> minus
        <|> dot
        <|> comma
        <|> lg
        <|> gt
        <|> loop

expr :: Parser Expr
expr = command <|> loop

program :: Parser Program
program = do
  exprList <- some expr
  return $ Program (S.fromList exprList)
