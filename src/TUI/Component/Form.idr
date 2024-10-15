||| Minimalist terminal UI framework.
|||
||| A form is an editor for a heterogenous list of values.
module TUI.Component.Form

import public Data.Vect
import public Data.Vect.Quantifiers

import TUI.View
import TUI.Component
import TUI.Component.Editor
import TUI.Component.HList
import TUI.Zipper.List
import Util


%default total


||| A field is a tuple label and a value editor.
export
record Field valueT where
  constructor F
  label : String
  value : Editor valueT

||| Construct a field for an editable type.
export
field
  :  Editable valueT
  => String
  -> Maybe valueT
  -> String
  -> Field valueT
field label value placeholder = F label (new value placeholder)

||| Paint a single field.
|||
||| The label and contents are justified according to the split value.
export
paintField
  : Editable valueT
  => Nat
  -> State
  -> Rect
  -> Field valueT
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

||| Handle input for a single field.
|||
||| This wraps the updates of the underlying `Editor`.
handleField
  :  Editable valueT
  => Component.Handler (Field valueT) valueT
handleField key self = case !(handle key self.value) of
  Continue state => update $ {value := !state} self
  Yield result   => yield result
  Exit           => exit
  Push top merge => push top (onMerge merge)
where
  ||| Proxy this modal merge back down to the underlying editor.
  onMerge : (Maybe a -> Editor valueT) -> Maybe a -> Field valueT
  onMerge merge result = {value := merge result} self

||| A form is a heterogenous container of labeled components.
|||
||| The components are layed out automatically. Exactly one form field
||| is focused at all times.
|||
||| The form has two states: editing and navigation. When in editing,
||| events pass to the selected form field. When in navigation mode,
||| events modify the form itself.
export
record Form (tys : Vect k Type) where
  constructor MkForm
  fields      : HList tys
  split       : Nat

||| When used, it aligns fields to the split value of the parent form.
implementation [splitAt]
     {split : Nat}
  -> Editable valueT
  => View (Field valueT)
where
  size self = vunion (MkArea split 1) (size self.value)
  paint = paintField split

||| View implementation for `Form`
implementation
     {k : Nat}
  -> {tys : Vect k Type}
  -> View (Form tys)
where
  size self = size self.fields
  paint state window self = do
    let contents = shrink window
    vline (contents.nw.shiftRight self.split) contents.size.height
    case state of
      Focused => box window
      _       => pure ()
    paint state contents self.fields

||| handle form event
handleForm : Component.Handler (Form tys) (HVect tys)

||| Construct a concrete form from a list of fields.
|||
||| @k        The number of fields.
||| @tys      The type of each field.
||| @editable Captures the editable implementation for each field value.
||| @fields   The concrete list of fields
export
new
  :  {k : Nat}
  -> {tys : Vect (S k) Type}
  -> {editable : All Editable tys}
  -> (fields : All Field tys)
  -> Form tys
new fields = MkForm (new 0 mkfields) maxLabelWidth
  where
    ||| Get the character width of the longest label in the form.
    maxLabelWidth : Nat
    maxLabelWidth = reduceAll max (length . label) 0 fields

    ||| Construct a component for a single field.
    |||
    ||| `splitAt` is the `View` implementation which correctly
    ||| aligns the field content to our split value.
    |||
    ||| Editable is the implementation required for `splitAt` to work.
    mkfield : (Field valueT, Editable valueT) -> Component valueT
    mkfield (f, e) = component @{splitAt {split = maxLabelWidth}} f handleField

    ||| Convert the input list of fields to a list of components.
    mkfields : All Component tys
    mkfields = mapProperty mkfield $ zipPropertyWith MkPair fields editable
