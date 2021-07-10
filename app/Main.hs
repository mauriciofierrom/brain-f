{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Options.Generic
import           System.Environment    (getArgs)
import           Text.Megaparsec       (runParser)
import           Text.Megaparsec.Error (showErrorComponent)

import qualified Data.Text             as T (Text, pack, strip)
import qualified Data.Text.IO          as TIO

import           Lib
import           Parsing

data Options w =
  Options
    { file   :: w ::: Maybe FilePath <?> "A .b or .bf file to execute"
    , source :: w ::: Maybe String <?> "Source code to execute"
    } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Wrapped)

main :: IO ()
main = do
  opts <- unwrapRecord "Execute brainfuck"
  case opts of
    (Options (Just _) (Just _)) -> putStrLn "You must provide either a program or a filepath, not both"
    (Options (Just f) _)         -> runIt =<< TIO.readFile f
    (Options _        (Just s)) -> runIt (T.strip . T.pack $ s)

runIt :: Text -> IO ()
runIt code =
  case runParser program [] code of
    Left err      -> print err
    Right program -> runProgram initialState program
