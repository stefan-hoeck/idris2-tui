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
||| @ Empty    An uninitialized value
||| @ Editing  Currently editing a value that may be incomplete.
||| @ Accepted Holding a legal value, possibly retaining previous editing state.
public export
data Editor valueT editorT
  = Empty String
  | Editing editorT (Maybe valueT)  String
  | Accepted valueT (Maybe editorT) String

||| Defines how to create an editor for a value.
public export
interface
     View valueT
  => View editorT
  => Controller editorT valueT
  => Editable valueT editorT | editorT
where
  constructor MkEditable
  fromValue  : valueT  -> editorT
  toValue    : editorT -> Maybe valueT
  blank      : editorT

||| Get the current value out of the editor.
|||
||| If in the empty state, returns Nothing. If in the editing state,
||| returns the current value if it parses, or the cached value if
||| available. If in the accepted state, returns the accepted value.
export
(.value)
  :  Editable valueT editorT
  => Editor valueT editorT
  -> Maybe valueT
(.value) (Empty        _) = Nothing
(.value) (Editing  x y _) = toValue x <+> y
(.value) (Accepted x y _) = Just x

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
  :  Editable valueT editorT
  => valueT
  -> String
  -> Editor valueT editorT
editing value ph = Editing (fromValue value) (Just value) ph

||| Transition to the editing state.
|||
||| This will re-use the previous editing state if one exists,
||| otherwise it will construct the editor from a value.
|||
||| Has no effect if already editing.
export
edit
  :  Editable valueT editorT
  => Editor valueT editorT
  -> Editor valueT editorT
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
  :  Editable valueT editorT
  => valueT
  -> Editor valueT editorT
  -> Editor valueT editorT
commit value self@(Editing e v ph) = Accepted value (Just e) ph
commit _ self = self

||| Leave the editing state, restoring previous value if present.
export
rollback
  :  Editable valueT editorT
  => Editor valueT editorT
  -> Editor valueT editorT
rollback (Editing _ Nothing  ph) = Empty ph
rollback (Editing e (Just v) ph) = Accepted v (Just e) ph
rollback self                    = self

||| Update the editor by applying the inner action.
liftUpdate
  :  Editable valueT editorT
  => editorT
  -> Editor valueT editorT
  -> Editor valueT editorT
liftUpdate next self = case self of
  Editing e v ph => Editing next v ph
  _              => self

||| Update the editor by applying the inner action.
liftEffect
  :  Editable valueT editorT
  => IO editorT
  -> Editor valueT editorT
  -> IO (Editor valueT editorT)
liftEffect next self = case self of
  Editing e v ph => do pure $ Editing !next v ph
  _              => pure self

export
Editable valueT editorT => View (Editor valueT editorT) where
  size (Empty placeholder)  = size placeholder
  size (Editing e _ _)      = size e
  size (Accepted value _ _) = size value

  paint state window self = case self of
    (Empty    placeholder) => paint state window placeholder
    (Editing  editor _ _)  => paint state window editor
    (Accepted value _ _)   => paint state window value

export
implementation
     Editable valueT editorT
  => Controller (Editor valueT editorT) valueT
where
  handle key self@(Editing e _ _) = case handle key e of
    Ignore           => Ignore
    (Yield (Just v)) => Do  $ commit v self
    (Yield Nothing)  => Do  $ rollback self
    (Do f)           => Do  $ liftUpdate f self
    (Run f)          => Run $ liftEffect f self
  handle Enter self = Do $ edit self
  handle _     _    = Ignore

export
editorViewImpl
  :  Editable valueT editorT
  => View (Editor valueT editorT)
editorViewImpl = %search
