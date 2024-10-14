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

export
delete : TextInput -> TextInput
delete = {chars $= delete}

export
goLeft : TextInput -> TextInput
goLeft = {chars $= goLeft}

export
goRight : TextInput -> TextInput
goRight = {chars $= goRight}

export
insert : Char -> TextInput -> TextInput
insert c = {chars $= insert c}

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
handle : Component.Handler TextInput String
handle Left      self = update $ goLeft self
handle Right     self = update $ goRight self
handle Delete    self = update $ delete self
handle (Alpha c) self = update $ insert c self
handle Enter     self = yield $ toString self
handle Escape    self = exit
handle _         self = ignore

export
textInput : String -> Component String
textInput string = active (fromString string) handle

{-
||| Make `String` `Editable` via `TextInput`
export
Editable String TextInput where
  fromValue = TextInput.fromString
  toValue   = Just . TextInput.toString
  blank     = TextInput.empty
  update    = handle

