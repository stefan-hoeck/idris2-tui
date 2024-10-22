module Main

import JSON
import JSON.Derive
import System
import System.File
import System.File.ReadWrite
import System.File.Virtual

%default total
%language ElabReflection


||| An Item in a user's todo-list
record Item where
  constructor I
  description : String
  completed   : Bool
%runElab derive "Item" [Show, Eq, Ord, FromJSON, ToJSON]

record TodoList where
  constructor L
  items : List Item
%runElab derive "TodoList" [Show, Eq, Ord, FromJSON, ToJSON]

covering
fromFile : String -> IO (Maybe TodoList)
fromFile path = do
  Right contents <- readFile path | Left err => pure Nothing
  case decode contents of
    Left  err      => pure Nothing
    Right contents => pure $ Just contents

covering
toFile : String -> TodoList -> IO ()
toFile path todolist = do
  ignore $ writeFile path $ encode todolist

covering
main : IO ()
main = do
  case !getArgs of
    [_]       => die "No path given"
    [_, path] => putStrLn $ show $ !(fromFile path)
    _         => die "Expected exactly one argument."
