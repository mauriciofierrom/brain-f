{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( Expr(..)
    , Program(..)
    , Cmd(..)
    , runProgram
    , initialState
    ) where

import Control.Monad (void)
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S (Seq(..), fromList, update, lookup, (|>), Seq((:<|)))

type Pointer = Int
type Byte = Int

data State = State { bytes :: S.Seq Byte, pointer :: Pointer } deriving (Eq, Show)

data Cmd = IncPointer
         | DecPointer
         | IncByte
         | DecByte
         | PrintByte
         | GetByte
         deriving Show

data Expr = Command Cmd | Loop (S.Seq Expr) deriving Show

newtype Program = Program (S.Seq Expr) deriving Show

runProgram :: State -> Program -> IO ()
runProgram state prog = void $ go prog state
  where
    go :: Program -> State -> IO State
    go (Program (x S.:<| xs)) st = go (Program xs) =<< runExpr x st
    go (Program S.Empty) st = return st

runExpr :: Expr -> State -> IO State
runExpr (Command cmd) state  = runCommand state cmd
runExpr (Loop lseq) state = go lseq state
  where
    go :: S.Seq Expr -> State -> IO State
    go (x S.:<| xs) st = go xs =<< runExpr x st
    go S.Empty st = if getCurrentByte st /= 0
                             then go lseq st
                             else return st

initialState :: State
initialState = State (S.fromList [0]) 0

runCommand :: State -> Cmd -> IO State
runCommand s@State{..} cmd =
  case cmd of
    IncPointer -> if length bytes == pointer + 1
                     then return $ State { bytes = bytes S.|> 0, pointer = pointer + 1 }
                     else return $ State { bytes = bytes, pointer = pointer + 1 }
    DecPointer -> if (-) pointer 1 >= 0
                     then return $ State { bytes = bytes, pointer = (-) pointer 1 }
                     else error "Out of bounds"
    IncByte    -> return $ incrementByte s
    DecByte    -> return $ decrementByte s
    PrintByte  -> printByte s >> return s
    GetByte -> getByte s

modifyByte :: (Integer -> Integer) -> State -> State
modifyByte f State{..} =
  let updatedByte = f $ toInteger $ getCurrentByte State{..}
   in State { bytes = S.update pointer (fromIntegral updatedByte) bytes, .. }

getCurrentByte :: State -> Byte
getCurrentByte State{..} = fromMaybe (error "Out of bounds. ")
                                     (S.lookup pointer  bytes)

printByte :: State -> IO ()
printByte st = putChar . chr . fromIntegral $ getCurrentByte st

getByte :: State -> IO State
getByte s@State{..} = do
  c <- getChar
  return $ modifyByte (const . fromIntegral . ord $ c) s

incrementByte :: State -> State
incrementByte = modifyByte (+1)

decrementByte :: State -> State
decrementByte = modifyByte (flip (-) 1)
