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
  choice        : Fin (length choices)

||| Get the selected value.
export
(.selected) : Exclusive itemT -> itemT
(.selected) self = index' self.choices self.choice

||| Select the next item.
export
prev : Exclusive itemT -> Exclusive itemT
prev = { choice $= predS }

||| Select the previous item.
export
next : Exclusive itemT -> Exclusive itemT
next = { choice $= finS }

||| Select the given index.
export
choose
  :  (self : Exclusive itemT)
  -> Fin (length self.choices)
  -> Exclusive itemT
choose self c = { choice := c } self

||| Model implementation for view
namespace Model

  ||| Actions valid on a choice object.
  |||
  ||| We could implement other actions, like filtering the set of
  ||| choices.
  public export
  data Action : Type where
    Prev   :        Action
    Next   :        Action
    Choose : Nat -> Action

  ||| Implement model for a leaf choice
  export
  Model (Exclusive itemT) itemT Action where
    update Prev       self = Left $ prev self
    update Next       self = Left $ next self
    update (Choose i) self = case natToFin i (length self.choices) of
      Just i => Left $ choose self i
      -- if the index is invalid, leave unchanged.
      Nothing => Left $ self

namespace View
  ||| Show the arrow indicator most appropriate for the given as a
  ||| hint to the user which keys will be effective.
  |||
  ||| - 0     : down arrow
  ||| - last  : the up arrow
  ||| - other : the up-down arrow.
  arrowForIndex : {k : Nat} -> (Fin k) -> String
  arrowForIndex FZ     = arrow Down
  arrowForIndex (FS n) = if FS n == last
    then arrow Up
    else arrow UpDown

  ||| Render a choice as a single list of items.
  export
  View itemT => View (Exclusive itemT) where
    size self = (MkArea 2 0) + (foldl Area.union (MkArea 0 0) $ size <$> self.choices)
    paint state window self = do
      withState state $ showTextAt window.nw (arrowForIndex self.choice)
      paint state (window.shiftRight 2) self.selected

{-
  ||| Render a choice as a vertical list of items.
  export
  [Vertical] {n : Nat} -> View itemT => View (Exclusive itemT) where
    size self = foldl Area.hunion (MkArea 0 0) $ size <$> self.choices
    paint state window self = loop self.choice window self.choices
      where
        adjustState : Fin (length self.choices) -> State -> State
        adjustState i Focused = case self.choice of
          Nothing     => Normal
          Just choice => if (i == choice) then Focused else Normal
        adjustState _ state = state

        loop : Fin (length self.choices) -> Rect -> List itemT -> IO ()
        loop i window [] = pure ()
        loop i window (x :: xs) = do
          window <- packTop (adjustState i state) window x
          loop (finS i) window xs
-}

namespace Controller

  ||| Provides basic selection of items via the arrow keys.
  export
  Controller (Exclusive itemT) (Response (Maybe itemT) Action) where
    handle Up     self = Do     Prev
    handle Down   self = Do     Next
    handle Left   self = Yield  Nothing
    handle Escape self = Yield  Nothing
    handle Enter  self = Yield  $ Just self.selected
    handle _      _    = Ignore

||| Create an exclusive choice component from a non-empty list of choices.
export
menu
  :  View itemT
  => (choices    : List itemT)
  -> {auto 0 prf : IsJust (natToFin 0 (length choices))}
  -> Exclusive itemT
menu choices = MkChoice choices $ fromJust $ natToFin 0 (length choices)


