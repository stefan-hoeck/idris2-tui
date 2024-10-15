module TUI.Component.HList

import Data.Fin
import Data.Vect
import Data.Vect.Quantifiers
import Data.SortedSet
import TUI.Component
import Util


%default total


||| A component that holds a heterogenous list of Components.
|||
||| Note: the `H` does not refer to orientation! An `HList` can be
||| drawn in a variety of ways.
|||
||| Exactly one item has focus at any given time. Supporting
||| anything else seems rather difficult.
export
record HList (tys : Vect k Type) where
  constructor MkHList
  items     : All Component tys
  selection : Fin k

||| Get the selected value.
export
(.selected)
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> (self  : HList tys)
  -> Component (index self.selection tys)
(.selected) self = get self.selection self.items

||| Select the next item.
export
prev
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> (self  : HList tys)
  -> HList tys
prev = { selection $= predS }

||| Select the previous item.
export
next
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> (self  : HList tys)
  -> HList tys
next = { selection $= finS }

||| Select the given index.
export
choose
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> Fin k
  -> (self  : HList tys)
  -> HList tys
choose c self = { selection := c } self

export
implementation
{k : Nat} -> {tys : Vect k Type} -> View (HList tys) where
  size = ?hole
  paint = ?hole2

||| Construct a new concrete HList
export
new
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> Fin k
  -> All Component tys
  -> HList tys
new choice components = MkHList components choice
