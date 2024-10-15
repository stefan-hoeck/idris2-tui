||| Minimalist terminal UI framework.
|||
||| A form is an editor for a heterogenous list of values.
module TUI.Component.Form

-- XXX: figure out what code breaks when these imports are made private.
import public Data.Vect
import public Data.Vect.Quantifiers
import TUI.Component
import TUI.Component.Editor
import TUI.Component.HList
import TUI.Layout
import TUI.View
import TUI.Zipper.List
import Util


%default total


||| A field is a labeled value editor.
export
record Field valueT where
  constructor F
  label : String
  value : Editor valueT

||| Contruct a single field.
|||
||| @label       The field label
||| @value       An optional initial value to populate the field.
||| @placeholder The placeholder string to use for an empty field.
|||
||| A field can be constructed for any type implementing `Editable`.
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
||| @split  The horizontal position to align field values to.
||| @state  The drawing state, as with any paint function.
||| @window The screen area to paint to, as with any paint function.
||| @self   The field value to paint.
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
||| This just wraps the updates of the underlying `Editor`.
handleField
  :  Editable valueT
  => Component.Handler (Field valueT) valueT
handleField key self = case !(handle key self.value) of
  Continue state => update $ {value := !state} self
  Yield result   => yield result
  Exit           => exit
  Push top merge => push top (onMerge merge)
where
  ||| Some boilerplate:
  ||| Proxy this modal merge back down to the underlying editor.
  onMerge : (Maybe a -> Editor valueT) -> Maybe a -> Field valueT
  onMerge merge result = {value := merge result} self

||| The focus state of a form
data FocusState = Edit | Submit | Cancel

||| A form is a heterogenous container of labeled components.
|||
||| Form fields are placed automatically.
|||
||| Exactly one form field is focused at all times.
|||
||| The form has two states: editing and navigation. When in editing,
||| events pass to the selected form field. When in navigation mode,
||| events modify the form itself.
export
record Form (tys : Vect k Type) where
  constructor MkForm
  fields : HList tys
  split  : Nat
  focus  : FocusState

||| When used, it aligns fields to the split value of the parent form.
implementation [splitAt]
     {split : Nat}
  -> Editable valueT
  => View (Field valueT)
where
  size self = vunion (MkArea split 1) (size self.value)
  paint = paintField split

||| View implementation for `Form`
export
implementation
     {k : Nat}
  -> {tys : Vect (S k) Type} -- see note below
  -> View (Form tys)
where
  size self = size self.fields
  paint state window self = do
    let contents = shrink window
    vline (contents.nw.shiftRight self.split) contents.size.height
    case state of
      Focused => box window
      _       => pure ()
    contents <- packTop  fieldsState contents self.fields
    contents <- packLeft cancelState contents "Cancel"
    contents <- packLeft submitState contents "Submit"
    pure ()
  where
    fieldsState : State
    fieldsState = case self.focus of
      Edit   => Focused
      _      => Normal

    submitState : State
    submitState = case self.focus of
      Submit => Focused -- XXX: show as disabled when form is invalid
      _      => Normal

    cancelState : State
    cancelState = case self.focus of
      Cancel => Focused
      _      => Normal

-- Note: it turns out `Vect (S k)` is key here. If we naively write
-- `Vect k`, Idris fails to resolve the `View` implementation in
-- HList.

||| handle form event
handleForm : {k : Nat} -> {tys : Vect k Type} -> Component.Handler (Form tys) (HVect tys)
handleForm key self = case (key, self.focus) of
  (Tab, _)        => update $ {fields $= prev} self
  (_, Edit)       => handleEdit
  (Enter, Submit) => case ?getValues of
    Nothing => ignore
    Just v  => yield v
  (Enter, Cancel) => exit
  (Escape, _)     => exit
  (Up, _)         => update $ {fields $= prev} self
  (Down, _)       => update $ {fields $= next} self
  _               => ignore
where
  handleEdit : IO $ Response (Form tys) (HVect tys)
  handleEdit = update $ {fields := ?hole} self

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
new fields = MkForm (new 0 mkfields) maxLabelWidth Edit
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
