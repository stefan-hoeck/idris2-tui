-- BSD 3-Clause License
--
-- Copyright (c) 2023, Brandon Lewis
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

||| Toggle an item's completed state.
toggle : Item -> Item
toggle = {completed $= not}

||| Set an item's description.
setDesc : String -> Item -> Item
setDesc d = {description := d}

||| Render the status of a Boolean as a Checkbox.
checkBox : Bool -> String
checkBox True  = "[+]"
checkBox False = "[ ]"

||| Get a visual representation an Item as a string.
(.summary) : Item -> String
(.summary) self = "\{checkBox self.completed} \{self.description}"

||| Implement `View` for Item, so it can be drawn to the screen.
|||
||| This is a requirement for Item to be used with `VList`.
View Item where
  size self = size self.summary
  paint state window self = paint state window self.summary

||| Create component for editing the todolist
|||
||| The path is used as the list header, so we know which file we're
||| editing.
todoList : String -> List Item -> Component Key (List Item)
todoList path items = vlist {
  header = path,
  items = items,
  onKey = onKey
} where
  ||| Update the selected list item description via a modal TextInput.
  editSelected : VList Item -> IO $ Response Key (VList Item) (List Item)
  editSelected self = case self.selected of
    Nothing => ignore
    Just item => push (textInput item.description) (onMerge item)
  where
    ||| Update the item description when the TextInput.
    onMerge : Item -> Maybe String -> VList Item
    onMerge _    Nothing  = self
    onMerge item (Just v) = update (setDesc v) self

  ||| Determines what to do when the user presses a key while this
  ||| component has focus.
  |||
  ||| The user can add a new component by pressing the + key. Space
  ||| toggles completion status. Arrow keys are used for
  ||| navigation. `q` will quit-and-save, while escape will quit
  ||| without saving. Other keys are ignored.
  onKey : Component.Handler (VList Item) (List Item) Key
  onKey (Alpha '+') self = update $ insert (I "New Item" False) self
  onKey (Alpha 'q') self = yield  $ toList self
  onKey (Alpha ' ') self = update $ update toggle self
  onKey Up          self = update $ goLeft self
  onKey Down        self = update $ goRight self
  onKey Enter       self = editSelected self
  onKey Escape      _    = exit
  onKey key         _    = ignore

||| Open a todo list file, and try to parse its contents into a list
||| of items. This doesn't do much in the way of error handling.
covering
fromFile : String -> IO (Maybe (List Item))
fromFile path = do
  Right contents <- readFile path | Left err => pure Nothing
  case decode contents of
    Left  err      => pure Nothing
    Right contents => pure $ Just contents

||| Save the given list of items to the given path as a JSON document.
covering
toFile : String -> List Item -> IO ()
toFile path todolist = do
  ignore $ writeFile path $ encode todolist

||| Open the given file, create the UI, and enter the application
||| mainloop. If parsing the file fails, an empty todolist will be
||| created.
covering
run : String -> IO ()
run path = do
  items <- fromFile path
  case !(runComponent @{centered} !getDefault (todoList path (fromMaybe [] items))) of
    Nothing => pure ()
    Just items => toFile path items

||| Program entry point. Parse arguments and enter the UI.
covering
main : IO ()
main = do
  case !getArgs of
    [_]       => die "No path given"
    [_, path] => run path
    _         => die "Expected exactly one argument."
