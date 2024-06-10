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


public export
data Action itemT actionT
  = Insert itemT
  | Update (itemT -> itemT)
  | Delete
  | Next
  | Prev
  | Home
  | Find (itemT -> Bool)
  | FindOrInsert (itemT -> Bool) (Lazy itemT)
  | Lift actionT

public export
0 Handler : Type -> Type -> Type
Handler itemT actionT = Key -> Response (List itemT) (Action itemT actionT)

export
record VList itemT actionT where
  constructor MkVList
  header    : String
  items     : Zipper itemT
  onKey     : Handler itemT actionT

export
empty : String -> Handler itemT actionT -> VList itemT actionT
empty header handler = MkVList header empty handler

export
fromList : String -> Handler itemT actionT -> List itemT -> VList itemT actionT
fromList header handler items = MkVList header (fromList items) handler

export
toList : VList itemT _ -> List itemT
toList self = toList self.items

export
length : VList _ _ -> Nat
length self = length self.items

||| Implement Model for an itemT that also implements Model
export
implementation
     Model itemT actionT
  => Model (VList itemT actionT) (VList.Action itemT actionT)
where
  update (Insert x)         = {items $= insert x}
  update (Update f)         = {items $= update f}
  update Delete             = {items $= delete}
  update Prev               = {items $= goLeft}
  update Next               = {items $= goRight}
  update Home               = {items $= rewind}
  update (Find p)           = {items $= find' p}
  update (FindOrInsert p d) = {items $= findOrInsert p d}
  update (Lift x)           = {items $= update (update x)}

||| itemT doesn't need to implement model.
|||
||| Signal this by using actionT = Void.
export
implementation Model (VList itemT Void) (VList.Action itemT Void)
where
  update (Insert x)         = {items $= insert x}
  update (Update f)         = {items $= update f}
  update Delete             = {items $= delete}
  update Prev               = {items $= goLeft}
  update Next               = {items $= goRight}
  update Home               = {items $= rewind}
  update (Find p)           = {items $= find' p}
  update (FindOrInsert p d) = {items $= findOrInsert p d}
  update (Lift x)             impossible

export
implementation
     View itemT
  => View (VList itemT actionT)
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

export
implementation
     Controller itemT valueT actionT
  => Controller (VList itemT actionT) (List itemT) (VList.Action itemT actionT)
where
  handle key self = self.onKey key

export
implementation
  Controller (VList itemT Void) (List itemT) (VList.Action itemT Void)
where
  handle key self = self.onKey key
