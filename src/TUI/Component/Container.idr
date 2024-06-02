||| COmmon logic for views which contain and manage subviews.
|||
||| The definitions in this module abstract over common UX patterns.
module TUI.Component.Container


import Data.Maybe
import TUI.Component
import TUI.Geometry
import TUI.Layout
import TUI.Painting
import TUI.View
import public Zipper


%default total


||| A container handles keyboard focus and navigation for its subviews.
public export
0 Container : Type -> Type
Container itemT = Zipper itemT

{-
namespace Model

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

  ||| Container-specific actions
  public export
  data Action actionT
    = Prev
    | Next
    | Lift actionT

  ||| Model implementation for when item is a nested model.
  export
  implementation
       Model itemT valueT actionT
    => Model (Container itemT) (List valueT) (Action actionT)
  where
    update GoForward self = Left $ goRight self
    update GoBack    self = Left $ goLeft self
    update Rewind    self = Left $ rewind self

  ||| Update the focused item in response to the given input event.
  |||
  ||| @key    : the event to handle.
  ||| @self   : the focus group we are operating on.
  export
  handleFocused
    :  Item itemT actionT
    => Key
    -> Container itemT
    -> Response (Container itemT) Action
  handleFocused key self = case cursor self of
    Nothing => Ignore
    Just item => case handle key item of
      Up           => Do GoBack
      Down         => Do GoForward
      key          => liftResponse key item
      Update next  => Update $ replace next self
      Do FocusNext => Update $ goRight self
      Do FocusPrev => Update $ goLeft self
      Do (Focus n) => Update $ seekTo n self
      Do (Lift  a) => Do a

-}

namespace View

  ||| Layout this container vertically
  export
  [vertical] View itemT => View (Container itemT) where
    size self = foldl
      (flip $ hunion . size)
      (MkArea 0 0)
      (Zipper.List.toList self)

    paint state window self = do
      let (left, cursor, right) = decompose self
      window <- paintVertical (demoteFocused state) window left
      window <- case cursor of
        Just cursor => packTop state window cursor
        Nothing => pure window
      ignore $ paintVertical (demoteFocused state) window right
