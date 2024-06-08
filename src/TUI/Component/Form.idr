||| Minimalist terminal UI framework.
|||
||| Editing and navigation for a set of labeled subviews.
module TUI.Component.Form

import public Data.Vect
import public Data.Vect.Quantifiers

import TUI.View
import Util
import Zipper


%default total

{-

||| Store the metadata for each form field.
|||
||| The field is either in the editing state, in which case events are
||| proxied down to the subview, or else it is in the default state,
||| in which case events operate on the parent form.
export
data Field actionT
  = Editing String (Dynamic actionT)
  | Default String (Dynamic actionT)

export
field : View innerT actionT => String -> innerT -> Field actionT
field label view = Default label (Dyn {stateT = innerT} view)

||| Project user data out of a field as the label.
(.label) : Field _ -> String
(.label) (Editing label _) = label
(.label) (Default label _) = label

||| Project the subview out of the field.
(.view) : Field actionT -> Dynamic actionT
(.view) (Editing _ view) = view
(.view) (Default _ view) = view

||| Place a field into the editing state.
enter : Field actionT -> Field actionT
enter self = Editing self.label self.view

||| Place a field in the default state.
exit : Field actionT -> Field actionT
exit self = Default self.label self.view

||| Paint a single field.
|||
||| The label and contents are justified according to the split value.
paintField : Nat -> State -> Rect -> Field _ -> IO ()
paintField split state window self = do
  let (left, right) = hdivide window split
  case (state, self) of
    (Focused, Editing label subview) => do
      sgr [SetStyle SingleUnderline]
      showTextAt left.nw label
      sgr [Reset]
      paint Focused right subview
    (Focused, Default label subview) => do
      reverseVideo
      showTextAt left.nw label
      sgr [Reset]
      paint Normal right subview
    (state, _) => do
      withState state $ showTextAt left.nw self.label
      paint state right self.view

||| Update the subview without changing its state.
updateSubview : Field actionT -> Dynamic actionT -> Field actionT
updateSubview (Editing l _) subview = Editing l subview
updateSubview (Default l _) subview = Default l subview

||| Handle input for a single field, depending on its state.
|||
||| If the field is in the Editing state, events are proxied to the
||| wrapped view.
|||
||| If the field is in the Default state, events are intepreted as
||| navigation commands.
handleField
  :  Key
  -> Field actionT
  -> Response (Field actionT) (Action actionT)
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

||| A form is a container of labeled views.
|||
||| The views are packed vertically, with labels visually aligned.
|||
||| One form field at a time may have focus.
export
record Form actionT where
  constructor MkForm
  fields      : Container (Field actionT)
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
