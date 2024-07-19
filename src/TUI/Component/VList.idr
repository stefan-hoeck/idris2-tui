||| An editable list of items with a vertical layout.
|||
||| XXX: There's no support for clipping or scrolling in this
||| framework -:- if there's one component which could *really* use
||| it, it's this one. Making this painlessly scrollable, even in a
||| hacky way, is a high priority for this library to become usable.
module TUI.Component.VList


import Data.Maybe
import TUI.Component
import TUI.Geometry
import TUI.Layout
import TUI.Painting
import TUI.View
import public TUI.Zipper.List


%default total

export
record VList itemT where
  constructor MkVList
  header    : String
  items     : Zipper itemT

export
empty : String -> VList itemT
empty header = MkVList header empty

export
fromList : String -> List itemT -> VList itemT
fromList header items = MkVList header (fromList items)

export
toList : VList itemT -> List itemT
toList self = toList self.items

export
length : VList _ -> Nat
length self = length self.items

export
lift
  :  (f : Zipper itemT -> Zipper itemT)
  -> (self : VList itemT)
  -> VList itemT
lift f = {items $= f}

export
implementation
     View itemT
  => View (VList itemT)
where
  size self = foldl
    (flip $ hunion . size)
    (MkArea 0 0)
    (Zipper.List.toList self.items)

  paint state window self = do
    let (left, cursor, right) = decompose self.items
    -- highlight header when we're at the beginning of the list.
    -- honestly, it's this tricky bit of logic which motivates this
    -- whole module.
    headerState <- case (left, cursor) of
      ([],  Nothing) => pure state
      _              => pure $ demoteFocused state
    -- draw the header
    window <- packTop headerState window self.header
    -- draw the rest of the zipper
    window <- paintVertical (demoteFocused state) window left
    window <- case cursor of
      Just cursor => packTop state window cursor
      Nothing => pure window
    ignore $ paintVertical (demoteFocused state) window right
