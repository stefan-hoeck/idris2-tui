||| A family of components for selecting from a list.
module TUI.Component.Menu


import Data.Fin
import Data.List
import Data.Nat
import TUI.Component
import TUI.Controller
import TUI.Layout
import TUI.Painting
import TUI.View
import Util


%default total

||| Represents an exclusive choice
export
record Exclusive itemT where
  constructor MkChoice
  choices       : List itemT
  choice        : Maybe (Fin (length choices))

||| Get the selected value.
export
(.selected) : Exclusive itemT -> Maybe itemT
(.selected) self = index' self.choices <$> self.choice

||| Select the next item.
export
prev : Exclusive itemT -> Exclusive itemT
prev = { choice $= map predS }

||| Select the previous item.
export
next : Exclusive itemT -> Exclusive itemT
next = { choice $= map finS }

||| Select the given index.
export
choose
  :  (self : Exclusive itemT)
  -> Fin (length self.choices)
  -> Exclusive itemT
choose self c = { choice := Just c } self

||| Unset the selection
export
unset : Exclusive itemT -> Exclusive itemT
unset = { choice := Nothing }

||| Model implementation for view
namespace Model

  ||| Actions valid on a choice object
  public export
  data Action : Type where
    Ignore :        Action
    Prev   :        Action
    Next   :        Action
    Choose : Nat -> Action

  export
  Model (Exclusive itemT) itemT Action where
    update Ignore     self = Left self
    update Prev       self = Left $ prev self
    update Next       self = Left $ next self
    update (Choose i) self = case natToFin i (length self.choices) of
      Just i => Left $ choose self i
      -- if the index is invalid, leave unchanged.
      Nothing => Left $ self
{-

namespace View
  ||| Show the arrow indicator most appropriate for the given as a
  ||| hint to the user which keys will be effective.
  |||
  ||| - 0     : down arrow
  ||| - last  : the up arrow
  ||| - other : the up-down arrow.
  arrowForIndex : {k : Nat} -> Fin k -> String
  arrowForIndex FZ     = arrow Down
  arrowForIndex (FS n) =
    if FS n == last
    then arrow Up
    else arrow UpDown

  ||| Render a choice as a single list of items.
  export
  [Single] {k : Nat} -> View itemT => View (Exclusive k itemT) where
    size self = (MkArea 2 0) + (foldl Area.union (MkArea 0 0) $ size <$> self.choices)
    paint state window self = do
      withState state $ showTextAt window.nw (arrowForIndex self.choice)
      paint state (window.shiftRight 2) self.selected

  ||| Render a choice as a vertical list of items.
  export
  [Vertical] {n : Nat} -> View itemT => View (Exclusive n itemT) where
    size self = foldl Area.hunion (MkArea 0 0) $ size <$> self.choices
    paint state window self = loop self.choice window self.choices
      where
        adjustState : Fin n -> State -> State
        adjustState i Focused = if (i == self.choice) then Focused else Normal
        adjustState _ state   = state

        loop : Fin n -> Rect -> Vect k itemT -> IO ()
        loop i window [] = pure ()
        loop i window (x :: xs) = do
          window <- packTop (adjustState i state) window x
          loop (predS i) window xs

namespace Controller

  ||| Provides basic selection of items via the arrow keys.
  export
  {n : Nat} -> Controller (Exclusive n itemT) (Action n) where
    handle Up     self = Prev
    handle Down   self = Next
    handle _      _    = Ignore


{-
||| Construct a menu from a vector of views
export
menu
  : {k : Nat}
  -> View innerT
  => (onChange : actionT)
  -> (choices  : Vect (S k) innerT)
  -> Menu innerT actionT
menu {k} onChange choices = MkMenu (S k) choices (natToFinLt 0) onChange
