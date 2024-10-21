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

||| Text-centric versions of geometric notions like Pos, Area, and
||| Rect.
module TUI.Geometry


import Derive.Prelude
import System
import TUI.Util


%language ElabReflection
%default total


||| The location of a character cell on the screen.
public export
record Pos where
  constructor MkPos
  x : Nat
  y : Nat
%runElab derive "Pos" [Eq, Ord, Show]

namespace Pos

  ||| Top-left screen corner
  public export
  origin : Pos
  origin = MkPos 1 1

  public export
  (.shiftRight) : Pos -> Nat -> Pos
  (.shiftRight) self offset = { x $= (+ offset) } self

  public export
  (.shiftLeft) : Pos -> Nat -> Pos
  (.shiftLeft) self offset = { x $= (`minus` offset) } self

  public export
  (.shiftDown) : Pos -> Nat -> Pos
  (.shiftDown) self offset = { y $= (+ offset) } self

  public export
  (.shiftUp) : Pos -> Nat -> Pos
  (.shiftUp) self offset = { y $= (`minus` offset) } self


||| The dimensions of a screen view
public export
record Area where
  constructor MkArea
  width : Nat
  height : Nat
%runElab derive "Area" [Eq, Ord, Show]

||| A rectangular screen region.
|||
||| This is a useful concept for layout. We can abstractly refer to
||| the different corners of the box.
public export
record Rect where
  constructor MkRect
  pos  : Pos
  size : Area
%runElab derive "Rect" [Eq, Ord, Show]

namespace OverloadsPosAreaPos
  ||| Adding a point to an area returns a new point.
  public export
  (+) : Pos -> Area -> Pos
  (+) (MkPos x y) (MkArea w h) = MkPos (x + w) (y + h)

  public export
  (-) : Pos -> Area -> Pos
  a - v = MkPos (a.x `minus` v.width) (a.y `minus` v.height)

namespace OverloadsPosPosArea
  public export
  (-) : Pos -> Pos -> Area
  (-) a b = MkArea (a.x `diff` b.x) (a.y `diff` b.y)

namespace OverloadsNatPosPos
  public export
  (*) : Nat -> Pos -> Pos
  scalar * pos = MkPos (scalar * pos.x) (scalar * pos.y)

namespace OverloadsPosNatPos
  public export
  (*) : Pos -> Nat -> Pos
  pos * scalar = MkPos (scalar * pos.x) (scalar * pos.y)

namespace OverloadsNatAreaArea
  public export
  (*) : Nat -> Area -> Area
  scalar * area = MkArea (scalar * area.width) (scalar * area.height)

namespace OverloadsAreaNatArea
  public export
  (*) : Area -> Nat -> Area
  area * scalar = MkArea (scalar * area.width) (scalar * area.height)

||| A width and height without a location.
namespace Area
  ||| Combine two areas to yield an area that contains both
  export
  union : Area -> Area -> Area
  union a b = MkArea (max a.width a.width) (max a.height b.height)

  ||| Pack areas vertically
  export
  hunion : Area -> Area -> Area
  hunion a b = MkArea (max a.width b.width) (a.height + b.height)

  ||| Pack areas horizontally
  export
  vunion : Area -> Area -> Area
  vunion a b = MkArea (a.width + b.width) (max a.height b.height)

  export
  (+) : Area -> Area -> Area
  a + b = MkArea (a.width + b.width) (a.height + b.height)

  export
  (-) : Area -> Area -> Area
  a - b = MkArea (a.width `minus` b.width) (a.height `minus` b.height)

||| Associated definitiosn for `Rect`.
namespace Rect
  ||| Northwest corner of the given rect
  export
  (.nw) : Rect -> Pos
  (.nw) b = b.pos

  ||| Northeast corner of the given rect
  export
  (.ne) : Rect -> Pos
  (.ne) b = b.pos + MkArea b.size.width 0

  ||| Northwest corner of the given rect
  export
  (.se) : Rect -> Pos
  (.se) b = b.pos + b.size

  ||| Southwest corner of the given rect
  export
  (.sw) : Rect -> Pos
  (.sw) b = b.pos + MkArea 0 b.size.height

  ||| The column of the left side of the rect
  export
  (.w) : Rect -> Nat
  (.w) b = b.pos.x

  ||| The column of the east side of the rect
  export
  (.e) : Rect -> Nat
  (.e) b = b.pos.x + b.size.width

  ||| The row of the north side of the rect
  export
  (.n) : Rect -> Nat
  (.n) b = b.pos.y

  ||| The row of the south side of the rect.
  export
  (.s) : Rect -> Nat
  (.s) b = b.pos.y + b.size.height

  ||| Return the smallest rectangle which contains the two points.
  export
  fromPoints : Pos -> Pos -> Rect
  fromPoints a b = MkRect (min a b) (a - b)

  ||| Split horizontally at `w` and return the pieces
  export
  hsplit : Rect -> Nat -> (Rect, Rect)
  hsplit b w =
    let
      left  = MkRect b.nw (MkArea w b.size.height)
      right = fromPoints (b.nw.shiftRight w) b.se
    in
      (left , right)

  ||| Like `hsplit`, but leave room for a vertical separator
  export
  hdivide : Rect -> Nat -> (Rect, Rect)
  hdivide b w =
    let
      left  = MkRect b.nw (MkArea w b.size.height)
      right = fromPoints (b.nw.shiftRight (w + 1)) b.se
    in
      (left , right)

  ||| Split vertically at `h` and return the pieces
  export
  vsplit : Rect -> Nat -> (Rect, Rect)
  vsplit b h =
    let
      top = MkRect b.nw (MkArea b.size.width h)
      bot = fromPoints (b.nw.shiftDown h) b.se
    in
      (top , bot)

  ||| Like `vsplit` but leave room for a horizontal separator.
  export
  vdivide : Rect -> Nat -> (Rect, Rect)
  vdivide b h =
    let
      top = MkRect b.nw (MkArea b.size.width h)
      bot = fromPoints (b.nw.shiftDown (h + 1)) b.se
    in
      (top , bot)

  ||| The smallest bounding box fully containing both rectangles.
  export
  union : Rect -> Rect -> Rect
  union a b =
    let
      tl = min a.nw b.nw
      br = min a.se b.se
    in
      fromPoints tl br

  ||| Rectangles form a semigroup with the union operation.
  export
  Semigroup Rect where
    (<+>) = union

  export
  (+) : Rect -> Area -> Rect
  r + v = MkRect (r.nw + v) r.size

  export
  (-) : Rect -> Area -> Rect
  r - v = MkRect (r.nw - v) r.size

  ||| Position contents relative to the top left of container.
  export
  relativeTo : (container : Rect) -> (contents : Rect) -> Rect
  relativeTo (MkRect nw _) (MkRect offset size) =
    let pos = MkPos (nw.x + offset.x) (nw.y + offset.y)
    in MkRect pos size

  public export
  (.shiftRight) : Rect -> Nat -> Rect
  (.shiftRight) self offset = { pos $= (+ (MkArea offset 0)) } self

  public export
  (.shiftLeft) : Rect -> Nat -> Rect
  (.shiftLeft) self offset = { pos := (self.pos - (MkArea offset 0)) } self

  public export
  (.shiftDown) : Rect -> Nat -> Rect
  (.shiftDown) self offset = { pos $= (+ (MkArea 0 offset)) } self

  public export
  (.shiftUp) : Rect -> Nat -> Rect
  (.shiftUp) self offset = { pos := (self.pos - (MkArea 0 offset)) } self


||| A common default size of terminal window.
export
r80x24 : Rect
r80x24 = MkRect origin (MkArea 80 24)

||| Get the window geometry
|||
||| XXX: handle SIGWINCH
export
screen : IO Rect
screen = do
  width  <- parseEnv "COLUMNS" 80 parsePositive
  height <- parseEnv  "LINES"  24 parsePositive
  pure $ MkRect origin $ MkArea width height
where
  parseEnv : String -> a -> (String -> Maybe a) -> IO a
  parseEnv key def parse = case !(getEnv key) of
    Just value => pure $ fromMaybe def $ parse value
    Nothing    => pure def

||| shrink the rectangle by the given size
export
inset : Rect -> Area -> Rect
inset self offset = {
  pos  $= (+ offset),
  size := self.size - (2 * offset)
} self

||| Inset a rectangle uniformly by one row and column.
export
shrink : Rect -> Rect
shrink r = inset r $ MkArea 1 1
