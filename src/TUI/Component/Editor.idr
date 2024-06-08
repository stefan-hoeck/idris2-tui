||| Minimalist terminal UI Framework
|||
||| A Component for editing values.
module TUI.Component.Editor


import Data.Maybe
import Data.String
import TUI.Component
import Util
import Zipper


%default total


||| Actions valid on TextInput
|||
||| @ Edit     Begin editing the inner value.
||| @ Rollback Restore the previous value, if any.
public export
data Action actionT
  = Edit
  | Commit
  | Rollback
  | Lift actionT

||| A component for editing arbitrary values.
|||
||| An `Editor` can be in one of three states:
||| @ Empty    An uninitialized value
||| @ Editing  Currently editing a value that may be incomplete.
||| @ Accepted Holding a legal value, possibly retaining previous editing state.
public export
data Editor valueT editorT actionT
  = Empty String
  | Editing editorT (Maybe valueT)
  | Accepted valueT (Maybe editorT)

||| Defines how to create an editor for a value.
public export
interface
     Model editorT actionT
  => View valueT
  => View editorT
  => Controller editorT valueT actionT
  => Editable valueT editorT actionT | editorT
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
  :  Editable valueT editorT actionT
  => Editor valueT editorT actionT
  -> Maybe valueT
(.value) (Empty _)       = Nothing
(.value) (Editing  x y)  = toValue x <+> y
(.value) (Accepted x y)  = Just x

||| Construct an empty editor.
export
empty : String -> Editor _ _ _
empty placeholder = Empty placeholder

||| Construct an editor initialized to a value.
export
accepted : a -> Editor a _ _
accepted value = Accepted value Nothing

||| Construct an editor in the editing state.
export
editing
  :  Editable valueT editorT actionT
  => valueT
  -> Editor valueT editorT actionT
editing value = Editing (fromValue value) (Just value)

||| Transition to the editing state.
|||
||| This will re-use the previous editing state if one exits,
||| otherwise it will construct the editor from a value.
|||
||| Has no effect if already editing.
export
edit
  :  Editable valueT editorT actionT
  => Editor valueT editorT actionT
  -> Editor valueT editorT actionT
edit (Empty _)             = Editing (blank {valueT = valueT}) Nothing
edit (Accepted v Nothing)  = Editing (fromValue v)             (Just v)
edit (Accepted v (Just e)) = Editing e                         (Just v)
edit self                  = self

||| Try to transition to the Accepted state with the current value.
|||
||| If this succeeds, preserves editing state. If this fails, remains
||| in the editing state.
export
commit
  :  Editable valueT editorT actionT
  => Editor valueT editorT actionT
  -> Editor valueT editorT actionT
commit self@(Editing e v) = case toValue e of
  Nothing => self
  Just v  => Accepted v (Just e)
commit self = self

||| Leave the editing state, restoring previous value if present.
export
rollback
  :  Editable valueT editorT actionT
  => Editor valueT editorT actionT
  -> Editor valueT editorT actionT
rollback (Editing _ Nothing)  = Empty "(empty)"
rollback (Editing e (Just v)) = Accepted v (Just e)
rollback self                 = self

||| Update the editor by applying the inner action.
lift
  :  Editable valueT editorT actionT
  => actionT
  -> Editor valueT editorT actionT
  -> Editor valueT editorT actionT
lift action self = case self of
  Editing e v => Editing (Model.update action e) v
  _     => self

export
implementation
     Editable valueT editorT actionT
  => Model (Editor valueT editorT actionT) (Editor.Action actionT)
where
  update Edit          self = edit        self
  update Commit        self = commit      self
  update Rollback      self = rollback    self
  update (Lift action) self = lift action self

export
Editable valueT editorT actionT => View (Editor valueT editorT actionT) where
  size (Empty placeholder) = size placeholder
  size (Editing e _)       = size e
  size (Accepted value _)  = size value

  paint state window self = case self of
    (Empty    placeholder) => paint state window placeholder
    (Editing  editor _)    => paint state window editor
    (Accepted value _)     => paint state window value

export
implementation
     Editable valueT editorT actionT
  => Controller (Editor valueT editorT actionT) valueT (Editor.Action actionT)
where
  handle key  (Editing editor _) = Lift <$> handle key editor
  handle Enter _                 = Do Edit
  handle _     _                 = Ignore

export
editorViewImpl
  :  Editable valueT editorT actionT
  => View (Editor valueT editorT actionT)
editorViewImpl = %search
