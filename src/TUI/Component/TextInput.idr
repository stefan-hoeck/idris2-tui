||| Minimalist terminal UI Framework
|||
||| An Editable String View
module TUI.Component.TextInput


import Data.String
import TUI.Component
import Util
import Zipper


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

namespace Model

  data Action
    = Ignore
    | Accept
    | Cancel
    | Delete
    | GoLeft
    | GoRight
    | Insert Char

  Model TextInput (Maybe String) Model.Action where
    update Ignore     self = Left self
    update Accept     self = Right $ Just $ toString self
    update Cancel     self = Right Nothing
    update Delete     self = Left $ { chars $= delete  } self
    update GoLeft     self = Left $ { chars $= goLeft  } self
    update GoRight    self = Left $ { chars $= goRight } self
    update (Insert c) self = Left $ { chars $= insert c} self

{-
||| Implement View for TextInput
export
View (TextInput actionT) actionT where
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
    moveTo rect.nw
    putStr $ kcap $ self.chars.left
    reverseVideo
    putStr $ case self.chars.right of
      [] => " "
      x :: _ => singleton x
    unreverseVideo
    putStr $ pack $ tail self.chars.right

  -- map keys to their obvious functions.
  handle Left      self = Update $ { chars $= goLeft  } self
  handle Right     self = Update $ { chars $= goRight } self
  handle Delete    self = Update $ { chars $= delete  } self
  handle (Alpha c) self = Update $ { chars $= insert c} self
  handle Enter     self = Do self.onChange
  handle Escape    self = Exit
  handle _         self = Update self
