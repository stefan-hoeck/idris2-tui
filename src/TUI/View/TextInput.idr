||| Minimalist terminal UI Framework
|||
||| An Editable String View
module TUI.View.TextInput


import Data.String
import TUI.View
import Util
import Zipper


%default total


||| An Editable String View.
export
record TextInput actionT where
  constructor TI
  chars     : Zipper Char
  onChange  : actionT

||| Construct an empty text input
export
empty : actionT -> TextInput actionT
empty onChange = TI empty onChange


||| Construct a text input from a string.
export
fromString : String  -> TextInput ()
fromString s = TI { chars = fromList $ unpack s, onChange = () }

||| get the string value from the text input.
export
toString : TextInput _ -> String
toString self = pack $ toList self.chars

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
  handle Enter     self = Run self.onChange
  handle Escape    self = FocusParent
  handle Tab       self = FocusNext
  handle _         self = Update self
