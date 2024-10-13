||| Minimalist terminal UI Framework
|||
||| A Component for editing values.
module TUI.Component.Editor


import Data.Maybe
import Data.String
import TUI.Component
import Util
import TUI.Zipper.List


%default total


||| A component for editing arbitrary values.
|||
||| An `Editor` can be in one of three states:
||| @ Empty    An uninitialized value.
||| @ Editing  Currently editing a value.
||| @ Accepted Holds a legal value, and possibly the last editor state.
public export
data Editor valueT
  = Empty    String
  | Editing  (Component valueT) (Maybe valueT)             String
  | Accepted valueT             (Maybe (Component valueT)) String

||| Defines how to create an editor for a value.
public export
interface
     View valueT
  => Editable valueT
where
  constructor MkEditable
  fromValue  : valueT -> Component valueT
  toValue    : Component valueT -> Maybe valueT
  blank      : Component valueT

{-
||| Get the current value out of the editor.
|||
||| If in the empty state, returns Nothing. If in the editing state,
||| returns the current value if it parses, or the cached value if
||| available. If in the accepted state, returns the accepted value.
export
(.value)
  :  Editable valueT
  => Editor valueT
  -> Maybe valueT
(.value) (Empty        _) = Nothing
(.value) (Editing  x y _) = toValue x <+> y
(.value) (Accepted x y _) = Just x

{-

||| Construct an empty editor.
export
empty : String -> Editor _ _
empty placeholder = Empty placeholder

||| Construct an editor initialized to a value.
export
accepted : a -> String -> Editor a _
accepted value placeholder = Accepted value Nothing placeholder

||| Construct an editor in the editing state.
export
editing
  :  Editable valueT
  => valueT
  -> String
  -> Editor valueT
editing value ph = Editing (fromValue value) (Just value) ph

||| Transition to the editing state.
|||
||| This will re-use the previous editing state if one exists,
||| otherwise it will construct the editor from a value.
|||
||| Has no effect if already editing.
export
edit
  :  Editable valueT
  => Editor valueT
  -> Editor valueT
edit (Empty               ph) = Editing (blank {valueT = valueT}) Nothing  ph
edit (Accepted v Nothing  ph) = Editing (fromValue v)             (Just v) ph
edit (Accepted v (Just e) ph) = Editing e                         (Just v) ph
edit self                     = self

||| Try to transition to the Accepted state with the current value.
|||
||| If this succeeds, preserves editing state. If this fails, remains
||| in the editing state.
export
commit
  :  Editable valueT
  => valueT
  -> Editor valueT
  -> Editor valueT
commit value self@(Editing e v ph) = Accepted value (Just e) ph
commit _ self = self

||| Leave the editing state, restoring previous value if present.
export
rollback
  :  Editable valueT
  => Editor valueT
  -> Editor valueT
rollback (Editing _ Nothing  ph) = Empty ph
rollback (Editing e (Just v) ph) = Accepted v (Just e) ph
rollback self                    = self

||| Update the editor by applying the inner action.
liftUpdate
  :  Editable valueT
  =>
  -> Editor valueT
  -> Editor valueT
liftUpdate next self = case self of
  Editing e v ph => Editing next v ph
  _              => self

||| Update the editor by applying the inner action.
liftEffect
  :  Editable valueT
  => IO
  -> Editor valueT
  -> IO (Editor valueT)
liftEffect next self = case self of
  Editing e v ph => do pure $ Editing !next v ph
  _              => pure self

export
Editable valueT => View (Editor valueT) where
  size (Empty placeholder)  = size placeholder
  size (Editing e _ _)      = size e
  size (Accepted value _ _) = size value

  paint state window self = case self of
    (Empty    placeholder) => paint state window placeholder
    (Editing  editor _ _)  => paint state window editor
    (Accepted value _ _)   => paint state window value

export
handle : Editable valueT => Handler (Editor valueT) valueT
handle key self@(Editing e _ _) = case (update key e) of
  foo => ?hewl




{-
  (Yield (Just v)) => Do  $ commit v self
  (Yield Nothing)  => Do  $ rollback self
  (Do f)           => Do  $ liftUpdate f self
  (Run f)          => Run $ liftEffect f self
  (Push t m)       => ?handle_later
handle Enter self = Do $ edit self
handle _     _    = Ignore
