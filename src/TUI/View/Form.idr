||| Minimalist terminal UI framework.
|||
||| Editing and navigation for a set of labeled subviews.
module TUI.View.Form

import public Data.Vect
import public Data.Vect.Quantifiers

import TUI.View
import Util


%default total


||| A single field in a form.
|||
||| It is a labeled value that wraps an inner view, capturing its
||| View implementation.
|||
||| The order of arguments is flipped here to allow for partial
||| application in `All`.
public export
record Field actionT valueT ty where
  constructor F
  label : String
  view : ty
  {auto impl : View ty actionT valueT}


||| Type alias for a heterogenous list of fields.
0 Fields : Vect k Type -> Type -> Type -> Type
Fields tys a v = All (Field a v) tys


||| Type alias for the response type we get for a single field.
0 FieldResponse : Field actionT valueT stateT -> Type
FieldResponse field = Response (Field actionT valueT stateT) actionT valueT

||| Get the area of the field's wrapped view, not including its
||| label.
viewSize : Field _ _ stateT -> Area
viewSize self = size @{self.impl} self.view

||| Update field's wrapped view in response to a key event.
handleView : Key -> (f : Field actionT valueT stateT) -> FieldResponse f
handleView k f = case handle @{f.impl} k (f.view) of
  Update new  => Update $ { view := new } f
  FocusParent => FocusParent
  FocusNext   => FocusNext
  Run effect  => Run effect

||| A form displays a set of views, each with a string label.
|||
||| One field has focus, and user input is routed to this sub-view.
export
record Form (tys : Vect k Type) actionT valueT where
  constructor MkForm
  fields : Fields tys actionT valueT
  focused : Fin k
  split : Nat
  editing : Bool
  contentSize : Area

parameters {k : Nat} {tys : Vect k Type}
  ||| Get the character width of the longest label in the form.
  maxLabelWidth : Fields tys _ _ -> Nat
  maxLabelWidth tys = reduceAll max (length . (.label)) 0 tys

  ||| Calculate the size the form widgets (not including the labels)
  export
  sizeViewsVertical : Fields tys _ _ -> Area
  sizeViewsVertical fields = reduceAll hunion (viewSize) (MkArea 0 0) fields


||| Render the form's fields vertically.
export
paintVertical
  : {k : Nat}
  -> {tys : Vect k Type}
  -> State
  -> Rect
  -> Form tys _ _ -> IO ()
paintVertical state window self = do
  loop 0 window self.fields
  where
    loop
      : {k : Nat}
      -> {tys : Vect k Type}
      -> Nat
      -> Rect
      -> Fields tys _ _ -> IO ()
    loop _  _ [] = pure ()
    loop i  window (x :: xs) = do
      let (top, bottom) = vsplit window (viewSize x).height
      let (left, right) = hsplit top    (self.split + 3)
      let left  = inset left  (MkArea 1 0)
      let right = inset right (MkArea 1 0)
      case (i == (finToNat self.focused), state) of
        (True, Focused) => do
          if self.editing
            then sgr [SetStyle SingleUnderline]
            else reverseVideo
          showTextAt left.nw x.label
          sgr [Reset]
          if self.editing
            then paint @{x.impl} Focused right x.view
            else paint @{x.impl} Normal  right x.view
        _ => do
          showTextAt left.nw x.label
          paint @{x.impl} Normal right x.view
      loop (S i) bottom xs


||| Dispatch keyboard input to the currently-focused subview.
|||
||| This has to handle `Escape`ing if the subview escapes.
export
handleNth
  : {k : Nat}
  -> {tys : Vect k Type}
  -> (i : Fin k)
  -> Key
  -> (fields: Fields tys actionT valueT)
  -> Response (Fields tys actionT valueT) actionT valueT
handleNth FZ key (f :: fs) = case handleView key f of
  Update new  => Update $ new :: fs
  FocusParent => FocusParent
  FocusNext   => FocusNext
  Run effect  => Run effect
handleNth (FS i) key (f :: fs) = case handleNth i key fs of
  Update fs   => Update $ f :: fs
  FocusParent => FocusParent
  FocusNext   => FocusNext
  Run effect  => Run effect


parameters {k : Nat} {tys : Vect k Type}
  ||| Move the form to the next focused value.
  export
  nextChoice : Form tys actionT valueT -> Form tys actionT valueT
  nextChoice = { focused $= finS }

  ||| Move the form to the previous focused value.
  export
  prevChoice : Form tys actionT valueT -> Form tys actionT valueT
  prevChoice = { focused $= predS }

  0 FocusedField : Form tys actionT valueT -> Type
  FocusedField {actionT, valueT} self =
    Field actionT valueT (index self.focused tys)

  public export -- XXX: need to be public?
  focusedField : (self : Form tys actionT valueT) -> FocusedField self
  focusedField self = get self.focused self.fields

  public export
  0 FormResponse : Form tys actionT valueT -> Type
  FormResponse {actionT, valueT} self =
    Response (Form tys actionT valueT) actionT valueT

  ||| Dispatch event to the selected field.
  |||
  ||| We may need to update our editing state in response.
  export
  handleEditing : Key -> (self : Form tys actionT valueT) -> FormResponse self
  handleEditing key self = case handleNth self.focused key self.fields of
    Update fields => Update $ { fields  := fields } self
    FocusParent   => Update $ { editing := False }  self
    FocusNext     => Update $ nextChoice            self
    Run effect    => Run effect

  ||| Handles events when in navigation mode.
  |||
  ||| Up/Down change the form focus, various other keys toggle the
  ||| editing state.
  export
  handleDefault : Key -> (self : Form tys actionT valueT) -> FormResponse self
  handleDefault Up     self = Update $ prevChoice self
  handleDefault Down   self = Update $ nextChoice self
  handleDefault Tab    self = Update $ nextChoice self
  handleDefault Right  self = Update $ { editing := True } self
  handleDefault Enter  self = Update $ { editing := True } self
  handleDefault Escape _    = FocusParent
  handleDefault Left   _    = FocusParent
  handleDefault _      self = Update self


  paintForm : State -> Rect -> Form tys _ _ -> IO ()
  paintForm state window self = do
    let height = window.size.height `minus` 2
    vline (window.nw + MkArea 0 1)                height
    vline (window.nw + MkArea (self.split + 3) 1) height
    case state of
      Focused => box window
      _       => pure ()
    paintVertical state (shrink window) self

  handleForm : Key -> (self : Form tys actionT valueT) -> FormResponse self
  handleForm key self = case self.editing of
    True  => handleEditing key self
    False => handleDefault key self


export
implementation
  {k : Nat}
  -> {tys : Vect (S k) Type}
  -> View (Form tys actionT valueT) actionT valueT
where
  size self = self.contentSize + MkArea self.split 1

  paint state window self = do
    let height = window.size.height `minus` 2
    vline (window.nw + MkArea 0 1)                height
    vline (window.nw + MkArea (self.split + 3) 1) height
    case state of
      Focused => box window
      _       => pure ()
    paintVertical state (shrink window) self

  -- dispatch events depending on editing state
  handle key self = handleForm key self


||| Construct a form from a list of field records
public export
form
  : {k : Nat}
  -> {tys : Vect (S k) Type}
  -> Fields tys actionT valueT
  -> Form tys actionT valueT
form fields = MkForm {
  fields      = fields,
  focused     = 0,
  split       = (maxLabelWidth fields),
  contentSize = (sizeViewsVertical fields),
  editing     = False
}
