||| Minimalist terminal UI framework.
|||
||| A form is an editor for a heterogenous list of values.
module TUI.Component.Form

import public Data.Vect
import public Data.Vect.Quantifiers

import TUI.View
import TUI.Component.Editor
import TUI.Controller
import Util
import TUI.Zipper.List


%default total


||| A field is a tuple of a label and a value editor.
export
record Field valueT editorT where
  constructor MkField
  label : String
  value : Editor valueT editorT

||| Construct a field for an editable type
export
field
  :  Editable valueT editorT
  => String
  -> (Maybe valueT)
  -> Field valueT editorT
field label Nothing      = MkField label (empty          "(empty)")
field label (Just value) = MkField label (accepted value "(empty)")

||| Lift a value to a field update.
export
update
  :  Editor valueT editorT
  -> Field valueT editorT
  -> Field valueT editorT
update next = {value := next}

||| Paint a single field.
|||
||| The label and contents are justified according to the split value.
export
paintField
  : Editable valueT editorT
  => Nat
  -> State
  -> Rect
  -> Field valueT editorT
  -> Context ()
paintField split state window self = do
  let (left, right) = hdivide window split
  case (state, self.value) of
    (Focused, Editing editor _ _) => do
      sgr [SetStyle SingleUnderline]
      showTextAt left.nw self.label
      sgr [Reset]
      paint Focused right self.value
    (Focused, _) => do
      reverseVideo
      showTextAt left.nw self.label
      sgr [Reset]
      paint Normal right self.value
    (state, _) => do
      withState state $ showTextAt left.nw self.label
      paint state right self.value

{-
||| Handle input for a single field, depending on its state.
|||
||| If the field is in the Editing state, events are proxied to the
||| wrapped view.
|||
||| If the field is in the Default state, events are intepreted as
||| navigation commands.
handleField
  :  Key
  -> Field valueT editorT
  -> Response (Field valueT editorT) valueT
handleField key self = case self of
  Editing _ subview => case handleDynamic key subview of
    Exit          => Update $ exit self
    Update inner  => Update $ updateSubview self inner
    Do     action => Do $ Lift action
  Default _ subview => case key of
    (Alpha c) => Update self
    Left      => Exit
    Right     => Update $ enter self
    Up        => Do FocusPrev
    Down      => Do FocusNext
    Delete    => Update self
    Enter     => Update $ enter self
    Tab       => Do FocusNext
    Escape    => Exit
-}

{-

||| A form is a container of labeled views.
|||
||| The views are packed vertically, with labels visually aligned.
|||
||| One form field at a time may have focus.
export
record Form actionT where
  constructor MkForm
  fields      : VList (Field T) actionT
  split       : Nat
  contentSize : Area

||| Implement View for Field
|||
||| This implementation depends on an implicit parent form, which
||| provides the split value.
%hint
forForm
  :  {auto parent : Form actionT}
  -> View (Field actionT) (Action actionT)
forForm {parent} = MkView {
  size = vunion (MkArea parent.split 1) . size . (.view),
  paint = paintField parent.split,
  handle = handleField
}

||| Implement View for Field
|||
||| This implementation depends on an implicit parent form, which
||| provides the split value.
export
%hint
splitAt
  :  (split : Nat)
  -> View (Field actionT) (Action actionT)
splitAt {split} = MkView {
  size = vunion (MkArea split 1) . size . (.view),
  paint = paintField split,
  handle = handleField
}

||| Get the character width of the longest label in the form.
maxLabelWidth : List (Field _) -> Nat
maxLabelWidth fields = foldl (flip $ max . length . (.label)) 0 fields

||| Render the form with its border
paintForm : State -> Rect -> Form actionT -> IO ()
paintForm state window self = do
  let contents = shrink window
  vline (contents.nw.shiftRight self.split) contents.size.height
  case state of
    Focused => box window
    _       => pure ()
  Container.paintVertical @{forForm} state contents self.fields

||| handle form event
handleForm
  :  Key
  -> Form actionT
  -> Response (Form actionT) actionT
handleForm key self = Container.liftResponse @{forForm} key update self.fields
  where
    update : Container (Field actionT) -> Form actionT
    update fields = { fields := fields } self

||| View implementation.
export
View (Form actionT) actionT where
  size self = self.contentSize
  paint     = paintForm
  handle    = handleForm

||| Construct a form from a list of field records
export
form : List (Field actionT) -> Form actionT
form fields =
  let
    split = maxLabelWidth fields
  in MkForm {
    fields      = goRight $ rewind $ fromList fields,
    split       = split,
    contentSize = sizeVertical @{forForm} fields
  }
