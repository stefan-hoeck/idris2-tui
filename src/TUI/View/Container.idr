||| COmmon logic for views which contain and manage subviews.
|||
||| The definitions in this module abstract over common UX patterns.
module TUI.View.Container


import Data.List1
import Data.Maybe
import TUI.Geometry
import TUI.Painting
import TUI.View

import public Zipper


%default total


||| Extends application-level action with container
public export
data Action actionT
  = FocusNext
  | FocusPrev
  | Focus Nat
  | Lift actionT

||| This is an interface that container items must satisfy.
public export
0 Item : Type -> Type -> Type
Item itemT actionT = View itemT (Action actionT)

||| A container handles keyboard focus and navigation for its subviews.
public export
0 Container : Type -> Type
Container itemT = Zipper itemT

||| Update the focused item in response to the given input event.
|||
||| @key    : the event to handle.
||| @self   : the focus group we are operating on.
export
handleFocused
  :  Item itemT actionT
  => Key
  -> Container itemT
  -> Response (Container itemT) actionT
handleFocused key self = case cursor self of
  Nothing => Update self
  Just item => case handle key item of
    Exit         => Exit
    Update next  => Update $ replace next self
    Do FocusNext => Update $ goRight self
    Do FocusPrev => Update $ goLeft self
    Do (Focus n) => Update $ seekTo n self
    Do (Lift  a) => Do a

||| Update the parent of a container, lifting the response.
|||
||| @key    : the event to handle.
||| @update : a callback to updates the parent view.
||| @self   : the focus group we are operating on.
export
liftResponse
  :  Item itemT actionT
  => (key        : Key)
  -> (updateWith : Container itemT -> outerT)
  -> (self       : Container itemT)
  -> Response outerT actionT
liftResponse key updateWith self = case cursor self of
  Nothing => Update (updateWith self)
  Just item => case handleFocused key self of
    Exit         => Exit
    Update next  => Update $ updateWith next
    Do a         => Do a

||| Calculate the size of the container
export
sizeVertical : Item itemT _ => Container itemT -> Area
sizeVertical fields = foldl
  (flip $ hunion . size)
  (MkArea 0 0)
  (Zipper.List.toList fields)

||| Paint the container by packing items vertically
export
paintVertical
  :  Item itemT actionT
  => State
  -> Rect
  -> Zipper itemT
  -> IO ()
paintVertical state window self = do
  let (left, cursor, right) = decompose self
  window <- paintVertical (demoteFocused state) window left
  window <- case cursor of
    Just cursor => packTop state window cursor
    Nothing => pure window
  ignore $ paintVertical (demoteFocused state) window right
