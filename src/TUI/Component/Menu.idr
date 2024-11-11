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

||| A family of components for selecting from a list.
module TUI.Component.Menu


import Data.Fin
import Data.List
import Data.List.Elem
import Data.Nat
import TUI.Component
import TUI.Layout
import TUI.Painting
import TUI.Util
import TUI.View


%default total


||| Represents an exclusive choice
export
record Exclusive itemT where
  constructor MkChoice
  choices       : List itemT
  choice        : Fin (length choices)

-- use (.dotted) version of these projection functions instead.
%hide Exclusive.choices
%hide Exclusive.choice

||| Construct a new Exclusive value with the given choice.
export
new
  :  (choices : List itemT)
  -> (choice : Fin (length choices))
  -> Exclusive itemT
new choices choice = MkChoice choices choice

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

||| Show the most appropriate indicator for the current choice.
|||
||| - 0     : down arrow
||| - last  : the up arrow
||| - other : the up-down arrow.
arrowForIndex : {k : Nat} -> (Fin k) -> String
arrowForIndex FZ     = arrow Down
arrowForIndex (FS n) = if FS n == last
  then arrow Up
  else arrow UpDown

||| A visually-compact Component representing an exclusive choice.
|||
||| It is rendered as a single value. The user can cycle through this
||| value via the arrow keys.
|||
||| One of the guarantees provided by this component is that the
||| yielded value *must* be an element of the given type.
|||
||| XXX: Ideally this guarantee would be expressed in the type system.
namespace Spinner

  ||| The View implementation renders the component value, plus an
  ||| arrow indicator, which signals whether the user is at the
  ||| beginning, middle, or end of the list of choices. Also, that
  ||| it's a spinner to begin with.
  |||
  ||| If the spinner is given more vertical space
  export
  View itemT => View (Exclusive itemT) where
    size self =
      let sizes   := View.size <$> self.choices
          content := foldl Area.union (MkArea 0 0) sizes
      in (MkArea 2 0) + content

    paint state window self = do
      withState state $ showTextAt window.nw (arrowForIndex self.choice)
      paint state (window.shiftRight 2) self.selected

  ||| Interaction with the user is via the up, down, enter, and escape
  ||| keys.
  |||
  ||| Up / Down cycle through the combinations
  export
  onKey : Component.Handler (Exclusive itemT) itemT Key
  onKey Up     self = update $ prev self
  onKey Down   self = update $ next self
  onKey Left   self = exit
  onKey Escape self = exit
  onKey Enter  self = yield self.selected
  onKey _      _    = ignore

  ||| Construct a spinner with the given numeric choice set as the
  ||| default.
  export
  spinner
    :  View itemT
    => (choices : List itemT)
    -> (choice  : Fin (length choices))
    -> Component Key itemT
  spinner {itemT} choices choice = component {
    state   = (new choices choice),
    handler = onKey,
    get     =  Just . (.selected)
  }

  ||| Construct a spinner using an explicit value.
  |||
  ||| This value must be provably an element of the list. This variant
  ||| is most useful when the list and element are constant.
  |||
  ||| Note: you may need to import Data.List in order to call this
  ||| function.
  export
  fromChoice
    :  View itemT
    => Eq itemT
    => (choices  : List itemT)
    -> (choice   : itemT)
    -> {auto 0 has : IsJust (findIndex (choice ==) choices)}
    -> Component Key itemT
  fromChoice choices choice {has} = spinner choices index
    where
      index : Fin (length choices)
      index = fromJust (findIndex (choice ==) choices)

  ||| Like `fromChoice`, but doesn't require a proof of membership
  ||| (returning a Maybe) instead.
  export
  maybeFromChoice
    :  View itemT
    => Eq itemT
    => (choices : List itemT)
    -> (choice  : itemT)
    -> Maybe (Component Key itemT)
  maybeFromChoice choices choice = spinner {
    choices = choices
  } <$> findIndex (choice ==) choices
