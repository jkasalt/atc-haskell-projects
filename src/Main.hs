module Main where

import Control.Monad (when)
import Data.List (intersperse)
import System.IO (hFlush, stdout)

data Inscription = O | X
    deriving (Show)

type Board = [[Maybe Inscription]]

newtype Turn = Turn Inscription

main :: IO ()
main = do
    putStrLn "Welcome to Tic Tac Toe!"
    loop $ replicate 3 (replicate 3 Nothing)

prettyBoard :: Board -> [String]
prettyBoard board = intersperse linesep (map prettyLine board)
  where
    linesep = replicate 5 '-'
    prettyLine = intersperse '|' . map inscrChar
    inscrChar i = case i of
        Nothing -> ' '
        Just X -> 'X'
        Just O -> 'O'

loop :: Board -> IO ()
loop board = do
    putStrLn $ unlines $ prettyBoard board

handleInput :: String -> IO Bool
handleInput "exit" = do
    putStrLn "Goodbye!"
    pure False
handleInput input = do
    putStrLn $ "You entered: " ++ input
    pure True
