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
||| @ Commit   Try to commit the given value.
||| @ Rollback Restore the previous value, if any.
public export
data Action valueT actionT
  = Edit
  | Lift (Response valueT actionT)

||| A component for editing arbitrary values.
|||
||| An `Editor` can be in one of three states:
||| @ Empty    An uninitialized value
||| @ Editing  Currently editing a value that may be incomplete.
||| @ Accepted Holding a legal value, possibly retaining previous editing state.
public export
data Editor valueT editorT
  = Empty String
  | Editing editorT (Maybe valueT)
  | Accepted valueT (Maybe editorT)

||| Defines how to create an editor for a value.
public export
interface
     View valueT
  => Component editorT valueT actionT
  => Editable valueT editorT
where
  fromValue : valueT  -> editorT
  toValue   : editorT -> Maybe valueT
  blank     : editorT

||| Construct an empty editor.
export
empty : String -> Editor _ _
empty placeholder = Empty placeholder

||| Construct an editor initialized to a value.
export
accepted : a -> Editor a _
accepted value = Accepted value Nothing

||| Construct an editor in the editing state.
export
editing
  :  Editable valueT editorT
  => valueT
  -> Editor valueT editorT
editing value = Editing (fromValue value) (Just value)

||| Transition to the editing state.
|||
||| This will re-use the previous editing state if one exits,
||| otherwise it will construct the editor from a value.
|||
||| Has no effect if already editing.
export
edit
  :  {auto impl : Editable valueT editorT}
  -> Editor valueT editorT
  -> Editor valueT editorT
edit (Empty _)             = Editing (blank @{impl}) Nothing
edit (Accepted v Nothing)  = Editing (fromValue v)   (Just v)
edit (Accepted v (Just e)) = Editing e               (Just v)
edit self                  = self

||| Try to transition to the Accepted state with the current value.
|||
||| If this succeeds, preserves editing state. If this fails, remains
||| in the editing state.
export
commit
  :  Editable valueT editorT
  => Editor valueT editorT
  -> Editor valueT editorT
commit self@(Editing e v) = case toValue e of
  Nothing => self
  Just v  => Accepted v (Just e)
commit self = self

||| Leave the editing state, restoring previous value if present.
export
rollback
  :  Editable valueT editorT
  => Editor valueT editorT
  -> Editor valueT editorT
rollback (Editing _ Nothing)  = Empty "(empty)"
rollback (Editing e (Just v)) = Accepted v (Just e)
rollback self                 = self

||| Update the editor by applying the inner action.
lift
  :  Editable valueT editorT
  => Response valueT actionT
  -> Editor valueT editorT
  -> Either (Editor valueT editorT) valueT
lift Ignore self           = Left self
lift (Yield Nothing)  self = Left $ rollback self
lift (Yield (Just x)) self = Left $ commit self
lift (Do action)      self = case self of
  Editing e v => case update action e of
    Left e => Left $ Editing e v
    Right v => Right v
  _ => Left self

||| Implement Component for Editor
export
implementation
     View valueT
  => Component editorT valueT actionT
  => Editable valueT editorT
  => Component (Editor valueT editorT) valueT (Editor.Action valueT actionT)
where
  update Edit        self = Left $ edit self
  update (Lift resp) self = lift resp self

  size (Empty placeholder) = size @{string} placeholder
  size (Editing editor _)  = size editor
  size (Accepted value _)  = View.size value

  paint state window (Empty ph)         = paint @{string} state window ph
  paint state window (Editing editor _) = paint state window editor
  paint state window (Accepted value _) = paint state window value

  handle key (Editing editor _) = Do $ Lift $ Component.handle key editor
  handle Enter  self = Do Edit
  handle _      self = Ignore
