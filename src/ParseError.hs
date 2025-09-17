{-# LANGUAGE NamedFieldPuns #-}

module ParseError where

import Display (Display, display)

data ParseError = MissingNum {commandName :: String} | MissingString | ExpectedANumber | UnknownCommand

instance Display ParseError where
    display MissingNum{commandName} =
        "For the `"
            ++ commandName
            ++ "` command, you need to specify the id. For example `"
            ++ commandName
            ++ " 4`. Try the `list` command to see all tasks and their id."
    display MissingString = "Missing string"
    display ExpectedANumber = "Expected a number"
    display UnknownCommand = "Unknonw command"
