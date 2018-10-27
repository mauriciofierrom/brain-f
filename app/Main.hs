{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T (pack)

import System.Environment (getArgs)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (showErrorComponent)

import Lib
import Parsing

main :: IO ()
main = do
  (x:_) <- getArgs
  case runParser program [] (T.pack x) of
    Left err -> putStrLn "Whatever error happened"
    Right program -> runProgram initialState program
