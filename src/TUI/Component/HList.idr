module TUI.Component.HList

import Data.Fin
import Data.Vect
import Data.Vect.Quantifiers
import Data.SortedSet
import TUI.Component
import TUI.Layout
import TUI.View
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

||| Get the type of the selection.
0 (.Selected)
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> (self  : HList tys)
  -> Type
(.Selected) {tys} self = Component (index self.selection tys)

||| Get the selected value.
export
(.selected)
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> (self  : HList tys)
  -> self.Selected
(.selected) self = get self.selection self.items

||| Select the next item.
export
prev
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> (self  : HList tys)
  -> HList tys
prev = {selection $= predS}

||| Select the previous item.
export
next
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> (self  : HList tys)
  -> HList tys
next = {selection $= finS}

||| Select the given index.
export
choose
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> Fin k
  -> (self  : HList tys)
  -> HList tys
choose c = {selection := c}

||| Route event to the selected component
export
handle
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> Component.Handler (HList tys) (HVect tys)
handle key self = case !(handle key self.selected) of
  Continue x => update $ {items := updateSelected !x} self
  Yield _    => exit
  Exit       => exit
  Push x f   => push x $ onMerge f
where
  updateSelected : self.Selected -> All Component tys
  updateSelected item = replaceAt self.selection item self.items

  onMerge : (Maybe a -> self.Selected) -> Maybe a -> HList tys
  onMerge merge result = {items := updateSelected (merge result)} self

export
implementation
     {k : Nat}
  -> {tys : Vect (S k) Type}
  -> View (HList tys)
where
  size self = reduceAll hunion size (MkArea 0 0) self.items
  paint state window self = do
    ignore $ recurse 0 window self.items
  where
    recurse : Fin (S k) -> Rect -> All Component a -> Context Rect
    recurse i window []        = pure $ window
    recurse i window (x :: xs) = recurse (finS i) !(packTop focus window x) xs
    where
      focus : State
      focus = case state of
        Focused => if i == self.selection then Focused else Normal
        other   => other

||| Construct a new concrete HList
export
new
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> Fin k
  -> All Component tys
  -> HList tys
new choice components = MkHList components choice
