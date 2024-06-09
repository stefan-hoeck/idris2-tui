||| A list component with a grid layout.
|||
||| The Row interface describes how to break an item up into
||| cells. The layout width of each column is the max width of every
||| cell in that column. The layout height of each row is the max
||| height of every cell in that row.
module TUI.Component.Table


import Data.Fin
import Data.Nat
import Data.Vect
import Data.Vect.Quantifiers
import TUI.Component
import Util


%default total


{-
record Table rowT actionT where
  rows    : Zipper rowT
  padding : Nat
  handle  : Key -> Response (List rowT) (Container.Action rowT actionT)
  tabstops: Vect

interface Row selfT actionT where
  cols   : Nat
  widths : Vect cols Nat

implementation
     Row rowT actionT
  => Model (Table rowT actionT) (Table.Action rowT actionT)
where
  update (Insert x)         = {rows $= insert x}
  update (Update f)         = {rows $= update f}
  update Delete             = {rows $= delete}
  update GoLeft             = {rows $= goLeft}
  update GoRight            = {rows $= goRight}
  update GoHome             = {rows $= rewind}
  update (Find p)           = {rows $= updateJust (find p)}
  update (FindOrInsert p d) = {rows $= findOrInsert p d}
  update (Lift x)           = {rows $= update (update x)}

Row rowT actionT => View (Table rowT actionT) where
  headers = 
  size    = ?sizehole
  paint   = ?painthole

implementation
  Controller
    (Table tys rowT actionT)
    (List rowT)
    (Table.Action rowT actionT)
where
  handle key self = self.handle key
