-- BSD 3-Clause License
--
-- Copyright (c) 2023, Brandon Lewis
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


module TUI.Component.HList

import Data.Fin
import Data.Vect
import Data.Vect.Quantifiers
import Data.SortedSet
import TUI.Component
import TUI.Layout
import TUI.View
import TUI.Util


%default total


||| A component that holds a heterogenous list of Components.
|||
||| Note: the `H` does not refer to orientation! An `HList` can be
||| drawn in a variety of ways.
|||
||| Exactly one item has focus at any given time. Supporting
||| anything else seems rather difficult.
public export
record HList (tys : Vect k Type) where
  constructor MkHList
  items     : All Component tys
  selection : Fin k

||| Get the type of the selection.
export
0 (.Selected)
  :  {k    : Nat}
  -> {tys  : Vect k Type}
  -> (self : HList tys)
  -> Type
(.Selected) {tys} self = Component (index self.selection tys)

||| Get the selected value.
export
(.selected)
  :  {k    : Nat}
  -> {tys  : Vect k Type}
  -> (self : HList tys)
  -> self.Selected
(.selected) self = get self.selection self.items

||| Select the next item.
export
prev
  :  {k    : Nat}
  -> {tys  : Vect k Type}
  -> (self : HList tys)
  -> HList tys
prev = {selection $= predS}

||| Select the previous item.
export
next
  :  {k    : Nat}
  -> {tys  : Vect k Type}
  -> (self : HList tys)
  -> HList tys
next = {selection $= finS}

||| Select the given index.
export
choose
  :  {k      : Nat}
  -> {tys    : Vect k Type}
  -> (choice : Fin k)
  -> (self   : HList tys)
  -> HList tys
choose c = {selection := c}

||| Get the current values for each component.
export
(.values)
  :  {k    : Nat}
  -> {tys  : Vect k Type}
  -> (self : HList tys)
  -> All Maybe tys
(.values) self = mapProperty (.value) self.items

||| Route event to the selected component.
|||
||| If a subcomponent yields, the value is ignored. Values of all
||| subcomponents can be retrieved by calling `(.values)` on a
||| concrete `HList`, or `(.value)` on an `HList` `Component`.
export
handle
  :  {k   : Nat}
  -> {tys : Vect k Type}
  -> Component.Handler (HList tys) (All Maybe tys) Key
handle key self = case !(handle key self.selected) of
  Continue x => update $ {items := updateSelected !x} self
  Yield v    => yield $ self.values
  Exit       => exit
  Push x f   => push x $ onMerge f
where
  updateSelected : self.Selected -> All Component tys
  updateSelected item = replaceAt self.selection item self.items

  onMerge : (Maybe a -> self.Selected) -> Maybe a -> HList tys
  onMerge merge result = {items := updateSelected (merge result)} self


namespace Views

  export
  implementation [vertical]
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

||| Construct an HList component that renders its contents vertically.
export
vertical
  :  {k   : Nat}
  -> {tys : Vect (S k) Type}
  -> Fin (S k)
  -> All Component tys
  -> Component (All Maybe tys)
vertical choice components = component @{vertical} {
  state   = (MkHList components choice),
  handler = handle,
  get     = (Just . (.values))
}
