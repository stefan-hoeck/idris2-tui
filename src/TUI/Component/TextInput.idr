||| Minimalist terminal UI Framework
|||
||| A Component for editing text.
module TUI.Component.TextInput


import Data.String
import TUI.Component
import TUI.Component.Editor
import Util
import TUI.Zipper.List


%default total


||| Actions valid on TextInput
public export
data Action
  = Delete
  | GoLeft
  | GoRight
  | Insert Char

||| An Editable String View.
export
record TextInput where
  constructor TI
  chars     : Zipper Char

||| Construct an empty text input
export
empty : TextInput
empty = TI empty

||| Construct a text input from a string.
export
fromString : String -> TextInput
fromString s = TI $ fromList $ unpack s

||| get the string value from the text input.
export
toString : TextInput -> String
toString self = pack $ toList self.chars

||| Implement Model for TextInput
export
Model TextInput Action where
  update Delete     self = {chars $= delete}   self
  update GoLeft     self = {chars $= goLeft}   self
  update GoRight    self = {chars $= goRight}  self
  update (Insert c) self = {chars $= insert c} self

||| Implement View for TextInput
export
View TextInput where
  -- Size is the sum of left and right halves
  size self = MkArea (length self.chars) 1

  -- when un-focused, just show the string value.
  paint Normal rect self = do
    showTextAt rect.nw (toString self)
  -- when disabled, show a faint string
  paint Disabled rect self = do
    sgr [SetStyle Faint]
    showTextAt rect.nw (toString self)
    sgr [Reset]
  -- when focused, show the cursor position in the string.
  paint Focused rect self = do
    showTextAt rect.nw $ kcap $ self.chars.left
    reverseVideo
    cheat $ putStr $ case self.chars.right of
      [] => " "
      x :: _ => singleton x
    unreverseVideo
    cheat $ putStr $ pack $ tail self.chars.right

||| Implement Component for TextInput.
export
Controller TextInput String Action where
  handle Left      self = Do GoLeft
  handle Right     self = Do GoRight
  handle Delete    self = Do Delete
  handle (Alpha c) self = Do $ Insert c
  handle Enter     self = Yield $ Just $ toString self
  handle Escape    self = Yield Nothing
  handle _         self = Ignore

||| Implement Component for TextInput.
export
Component TextInput String Action where

||| Make `String` `Editable` via `TextInput`
export
Editable String TextInput Action where
  fromValue = TextInput.fromString
  toValue   = Just . TextInput.toString
  blank     = TextInput.empty
