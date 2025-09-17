module Display where

class Display a where
    display :: a -> String

(<+>) :: String -> String -> String
a <+> b = a <> " " <> b
