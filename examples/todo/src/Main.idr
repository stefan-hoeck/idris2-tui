module Main

import JSON
import JSON.Derive
import System
import System.File
import System.File.ReadWrite
import System.File.Virtual
import TUI
import TUI.MainLoop
import TUI.MainLoop.Default
import TUI.Zipper.List

%default total
%language ElabReflection


||| An Item in a user's todo-list
record Item where
  constructor I
  description : String
  completed   : Bool
%runElab derive "Item" [Show, Eq, Ord, FromJSON, ToJSON]

toggle : Item -> Item
toggle = {completed $= not}

View Item where
  size self = size self.description
  paint state window self = paint state window summary
    where
      status : String
      status = case self.completed of
        True  => "[+]"
        False => "[ ]"

      summary : String
      summary = "\{status} \{self.description}"

||| A component for editing the todolist
todoList : List Item -> Component (List Item)
todoList items = component (fromList header items) onKey (Just . toList) where
  header : String
  header = "Description"

  editSelected : VList Item -> IO $ Response (VList Item) (List Item)
  editSelected self = case self.selected of
    Nothing => ignore
    Just item => push (textInput item.description) (onMerge item)
  where
    onMerge : Item -> Maybe String -> VList Item
    onMerge _    Nothing  = self
    onMerge item (Just v) = insert ({description := v} item) self

  onKey : Component.Handler (VList Item) (List Item) Key
  onKey (Alpha '+') self = update $ insert (I "New Item" False) self
  onKey (Alpha 'q') self = yield  $ toList self
  onKey (Alpha ' ') self = update $ update toggle self
  onKey Up          self = update $ goLeft self
  onKey Down        self = update $ goRight self
  onKey Enter       self = editSelected self
  onKey Escape      _    = exit
  onKey key         _    = ignore

covering
fromFile : String -> IO (Maybe (List Item))
fromFile path = do
  Right contents <- readFile path | Left err => pure Nothing
  case decode contents of
    Left  err      => pure Nothing
    Right contents => pure $ Just contents

covering
toFile : String -> List Item -> IO ()
toFile path todolist = do
  ignore $ writeFile path $ encode todolist

covering
run : String -> IO ()
run path = do
  items <- fromFile path
  case !(runComponent !getDefault (todoList (fromMaybe [] items))) of
    Nothing => pure ()
    Just items => toFile path items

covering
main : IO ()
main = do
  case !getArgs of
    [_]       => die "No path given"
    [_, path] => run path
    _         => die "Expected exactly one argument."
