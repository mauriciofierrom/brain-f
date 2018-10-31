{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T (pack, strip, Text)

import System.Environment (getArgs)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (showErrorComponent)

import Lib
import Parsing

main :: IO ()
main = do
  args <- getArgs
  case prepareArgs args of
    Just code -> case runParser program [] code of
                   Left err -> putStrLn "Whatever error happened"
                   Right program -> runProgram initialState program
    Nothing   -> putStrLn "You must enter the code you want to execute"

prepareArgs :: [String] -> Maybe T.Text
prepareargs (x:_) = Just (T.strip . T.pack $ x)
prepareArgs [] = Nothing
