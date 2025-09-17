module Parsers where

import Command (Command (..), UpdateMode (..))
import qualified Data.Bifunctor as Bifun
import ParseError (ParseError (ExpectedANumber, MissingNum, MissingString, UnknownCommand, commandName))
import Task (Priority (..), TaskId (TaskId))
import Text.Read (readEither)

mapErr :: (e -> e') -> Either e t -> Either e' t
mapErr = Bifun.first

readTaskId :: String -> Either ParseError (TaskId Int)
readTaskId n = mapErr (const ExpectedANumber) (TaskId <$> readEither n)

parseCommand :: String -> Either ParseError Command
parseCommand s = case words s of
    "help" : _ -> Right Help
    "exit" : _ -> pure Exit
    "list" : _ -> pure List
    "list-done" : _ -> pure ListDone
    "next" : _ -> pure Next
    ["new"] -> Left MissingString
    ["complete"] -> Left MissingNum{commandName = "complete"}
    ["delete"] -> Left MissingNum{commandName = "delete"}
    ["edit"] -> Left MissingNum{commandName = "edit"}
    "complete" : n : _ -> Update <$> readTaskId n <*> pure Complete
    "delete" : n : _ -> Update <$> readTaskId n <*> pure Delete
    "edit" : n : rest -> Update <$> readTaskId n <*> pure (Edit $ unwords rest)
    "new" : prio : rest ->
        let
            p = case prio of
                "priority=low" -> Just Low
                "priority=medium" -> Just Medium
                "priority=high" -> Just High
                _ -> Nothing
            desc = unwords $ case p of
                Just _ -> rest
                Nothing -> prio : rest
         in
            pure $ New p desc
    _ -> Left UnknownCommand
