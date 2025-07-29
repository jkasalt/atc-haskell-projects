module Main where

import Control.Monad (when)
import Data.Kind (Type)
import Data.List (intersperse, transpose)
import GHC.Base (Alternative ((<|>)), join)
import GHC.List ((!?))
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

data Inscription = O | X
    deriving (Show, Eq)

data Tile = Free | Busy Inscription
    deriving (Show, Eq)

type Board = [[Tile]]

newtype Turn = Turn Inscription
    deriving (Show)

newtype Winner = Winner Inscription deriving (Show)

pass :: Turn -> Turn
pass (Turn X) = Turn O
pass (Turn O) = Turn X

main :: IO ()
main = do
    putStrLn "Welcome to Tic Tac Toe!"
    doLoop >>= \(_, winner) -> case winner of
        Nothing -> putStrLn "Nobody won"
        Just (Winner X) -> putStrLn "X won"
        Just (Winner O) -> putStrLn "O won"
    return ()

emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Free)

doLoop :: IO (Board, Maybe Winner)
doLoop = loop (Turn X) emptyBoard

prettyBoard :: Board -> [String]
prettyBoard board = intersperse linesep (map prettyLine board)
  where
    linesep = replicate 5 '-'
    prettyLine = intersperse '|' . map inscrChar
    inscrChar i = case i of
        Free -> ' '
        Busy X -> 'X'
        Busy O -> 'O'

data Command = Place (Int, Int) | Exit

parseInput :: String -> Maybe Command
parseInput s = case words s of
    [n, m] -> Place <$> ((,) <$> readMaybe n <*> readMaybe m)
    "exit" : _ -> Just Exit
    _ -> Nothing

getInput :: IO (Maybe Command)
getInput = parseInput <$> getLine

get :: Board -> (Int, Int) -> Maybe Tile
get board (y, x) = do
    line <- board !? y
    line !? x

parenthesized :: String -> String
parenthesized s = "(" ++ s ++ ")"

data HandleAction = Err String | NewTurn Board | DoExit

handleAction :: Turn -> Board -> Command -> HandleAction
handleAction turn board command =
    case command of
        Place pos -> case get board pos of
            Nothing -> Err "Out of bounds"
            Just (Busy X) -> Err "that tile is already occupied by X"
            Just (Busy O) -> Err "that tile is already occupied by O"
            Just Free -> let newBoard = place board turn pos in NewTurn newBoard
        Exit -> DoExit

loop :: Turn -> Board -> IO (Board, Maybe Winner)
loop turn board = do
    putStrLn $ unlines $ prettyBoard board
    putStr $ parenthesized (show turn) ++ " Enter command: "
    hFlush stdout
    case checkWinner board of
        w@(Just _) -> return (board, w)
        _ -> do
            command <- getInput
            case handleAction turn board <$> command of
                Nothing -> do
                    putStrLn "Failed to parse command"
                    loop turn board
                Just (Err e) -> do
                    putStrLn e
                    loop turn board
                Just (NewTurn newBoard) -> loop (pass turn) newBoard
                Just DoExit -> do
                    putStrLn "goodbye!"
                    return (board, Nothing)

checkRows :: [[Tile]] -> Maybe Inscription
checkRows rows
    | any (allSame X) rows = Just X
    | any (allSame O) rows = Just O
    | otherwise = Nothing
  where
    allSame inscr = all (== Busy inscr)

checkCols :: [[Tile]] -> Maybe Inscription
checkCols = checkRows . transpose

checkDiags :: [[Tile]] -> Maybe Inscription
checkDiags rows =
    let
        diag1 = [rows !! y !! x | (y, x) <- zip [0 .. numRows - 1] [0 .. minNumCols - 1]]
        diag2 = [rows !! y !! x | (y, x) <- zip [numRows - 1, numRows - 2 ..] [0 .. minNumCols - 1]]
     in
        checkRows [diag1, diag2]
  where
    numRows = length rows
    minNumCols = minimum (length <$> rows)

checkWinner :: Board -> Maybe Winner
checkWinner board = Winner <$> (checkRows board <|> checkCols board <|> checkDiags board)

place :: Board -> Turn -> (Int, Int) -> Board
place board (Turn inscr) (y, x) =
    let
        (top, row : bottom) = splitAt y board
        (h, _ : t) = splitAt x row
        newRow = h ++ Busy inscr : t
     in
        top ++ newRow : bottom
