module Main where

import System.IO (hFlush, stdout)
import Control.Monad (when)
import Text.Read (readMaybe)

newtype TaskId = TaskId Int
  deriving (Show)

data Command = Exit | List | Complete TaskId | Delete TaskId | Edit TaskId String
  deriving (Show)

data Task = Task {
    taskId :: TaskId,
    description :: String
}

parseCommand :: String -> Maybe Command
parseCommand s = case words s of
  "exit":_ -> Just Exit
  "list":_ -> Just List
  "complete":n:_ -> Complete . TaskId <$> readMaybe n
  "delete":n:_ -> Delete . TaskId <$> readMaybe n
  "edit":n:rest -> (Edit . TaskId <$> readMaybe n) <*> pure (unwords rest)
  _ -> Nothing

main :: IO ()
main = do
  putStrLn "Welcome to my TODO List Manager!"
  loop

loop :: IO ()
loop = do
  putStr "Enter command: "
  hFlush stdout
  input <- getLine
  isLooping <- handleInput input
  when isLooping loop

handleInput :: String -> IO Bool
handleInput "exit" = do
  putStrLn "Goodbye!"
  pure False
handleInput input = do
  putStrLn $ "You entered: " ++ input
  pure True
