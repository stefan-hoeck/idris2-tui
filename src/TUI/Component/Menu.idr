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
import Data.Nat
import TUI.Component
import TUI.Layout
import TUI.Painting
import TUI.Util
import TUI.View


%default total


||| XXX: this could easily support multiple selection!


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

namespace Spinner

  export
  View itemT => View (Exclusive itemT) where
    size self =
      let sizes   := View.size <$> self.choices
          content := foldl Area.union (MkArea 0 0) sizes
      in (MkArea 2 0) + content

    paint state window self = do
      withState state $ showTextAt window.nw (arrowForIndex self.choice)
      paint state (window.shiftRight 2) self.selected

  export
  handle : Component.Handler (Exclusive itemT) itemT Key
  handle Up     self = update $ prev self
  handle Down   self = update $ next self
  handle Left   self = exit
  handle Escape self = exit
  handle Enter  self = yield self.selected
  handle _      _    = ignore

  export
  new
    :  (choices    : List itemT)
    -> {auto 0 prf : IsJust (natToFin 0 (length choices))}
    -> Exclusive itemT
  new choices = MkChoice choices $ fromJust $ natToFin 0 (length choices)

  export
  spinner
    :  View itemT
    => (choices    : List itemT)
    -> {auto 0 prf : IsJust (natToFin 0 (length choices))}
    -> Component itemT
  spinner {itemT} choices = component {
    state   = (new choices),
    handler = handle,
    get     =  Just . (.selected)
  }
