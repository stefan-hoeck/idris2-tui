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
