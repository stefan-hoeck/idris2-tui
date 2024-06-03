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
interface Editable valueT editorT actionT
  | valueT
  , editorT
where
  constructor MkEditable
  component : Component editorT valueT actionT
  view      : View valueT
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
  :  Editable valueT editorT actionT
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
  :  Editable valueT editorT actionT
  => Editor valueT editorT
  -> Editor valueT editorT
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
  => Editor valueT editorT
  -> Editor valueT editorT
commit self@(Editing e v) = case toValue e of
  Nothing => self
  Just v  => Accepted v (Just e)
commit self = self

||| Leave the editing state, restoring previous value if present.
export
rollback
  :  Editable valueT editorT actionT
  => Editor valueT editorT
  -> Editor valueT editorT
rollback (Editing _ Nothing)  = Empty "(empty)"
rollback (Editing e (Just v)) = Accepted v (Just e)
rollback self                 = self

||| Update the editor by applying the inner action.
lift
  :  Editable valueT editorT actionT
  => Response valueT actionT
  -> Editor valueT editorT
  -> Either (Editor valueT editorT) valueT
lift Ignore self           = Left self
lift (Yield Nothing)  self = Left $ rollback self
lift (Yield (Just x)) self = Left $ commit self
lift (Do action)      self = case self of
  Editing e v => case Component.update @{component} action e of
    Left e => Left $ Editing e v
    Right v => Right v
  _ => Left self

||| Implement Component for Editor
export
implementation
     {impl : Editable valueT editorT actionT}
  -> Component (Editor valueT editorT) valueT (Editor.Action valueT actionT)
where
  update Edit        self = Left $ edit self
  update (Lift resp) self = lift resp self

  size (Empty placeholder) = size @{string}            placeholder
  size (Editing e _)       = size @{component @{impl}} e
  size (Accepted value _)  = size @{view      @{impl}} value

  paint state window self = case self of
    (Empty    placeholder) => paint @{string}            state window placeholder
    (Editing  editor _)    => paint @{component @{impl}} state window editor
    (Accepted value _)     => paint @{view @{impl}}      state window value

  handle key (Editing editor _) = Do $ Lift $ handle @{component @{impl}} key editor
  handle Enter  self = Do Edit
  handle _      self = Ignore
