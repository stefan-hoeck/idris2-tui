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

||| Immediate-mode TUI graphics.
|||
||| These routines are slightly higher-level than that provided by
||| `Control.ANSI`. In particular, we use types from `TUI.Geometry`.
|||
||| These routines operate on an opaque `Context` which theoretically
||| tracks the draw state, allowing for scope-based mangement of draw
||| state (position, color, font, style, etc). In practice everything
||| is written directly to stdout. Baby steps.
|||
||| I would like to support client code which looks like:
|||
||| ```idris
||| withSGR [Reverse] $ do
|||    window <- packTop state window self.label
|||    withClip window $ do
|||      withTransform self.scrollPos $ \window => do
|||        ignore $ paintVertical state window self.items
|||      ...
||| ```
|||
||| In certain situations, the rightward drift is a small price to pay
||| for clear scoping of draw operations.
|||
||| There is no support for *clipping*, and so there's no support for
||| scrolling. It's still early days.
module TUI.Painting


import public Control.ANSI
import Data.String
import System
import System.File
import System.File.Virtual
import public TUI.Geometry


%default total


||| A terminal image is just a giant string, and the terminal is
||| responsible for drawing it.
|||
||| We try not to make too many assumptions about how the terminal
||| does this, as it can vary. But, in general, concatenating two
||| strings will combine the two images.
|||
||| As an optimization, we represent lists of string fragments, which
||| we merge with `fastConcat` prior to rendering.
0 Image : Type
Image = SnocList String

||| Represent the regions to mask off from the clipped image plane
0 ClipMask : Type
ClipMask = List Rect

||| True for cells chich should *not* be erased during clipping.
(.contains) : ClipMask -> Pos -> Bool
(.contains) mask pos = Prelude.any (flip (.contains) $ pos) mask

||| A context for drawing to the terminal.
|||
||| The context provides two drawing layers. Default is the top-most
||| layer, and is always drawn completely. The Masked layer is used to
||| implement clipped drawing regions, as its name would suggest.
export
record Context a where
  constructor C
  masked  : Image
  mask    : ClipMask
  content : Image
  value   : a

||| Write the given unescaped string to the context.
export
putStr : String -> Context ()
putStr string = C [<] [] [< string] ()

||| Write the given unescaped character to the context.
export
putChar : Char -> Context ()
putChar c = putStr $ singleton c

||| Erase the the masked area of the screen.
|||
||| This naive algorithm should be optimized later if need be.
clearMasked : Area -> ClipMask -> IO ()
clearMasked _      []   = pure ()
clearMasked window mask = do
  -- reset all characters
  putStr $ escapeSGR [Reset]
  -- write a blank character to any cell that is masked off.  we have
  -- to do this without the benefit of any abstractions defined here,
  -- since this runs in IO
  --
  -- this could be optimized to look for runs of blank space, and
  -- issue screen clear operations instead, but we'll wait until this
  -- is proven to be a bottleneck.
  for_ [1 .. window.width] $ \i => do
    for_ [1 .. window.height] $ \j => do
      let pos = MkPos i j
      case mask.contains (MkPos i j) of
        True  => pure ()
        False => do
          putStr $ cursorMove j i
          putChar ' '
  -- in case our window is smaller than the true screen size
  -- (e.g. because we don't handle SIGWINCH), clear any contents from
  -- the clip region that happen to lie outside the screen bounds.
  putStr $ eraseScreen End

||| Paint the context to stdout.
|||
||| Draws the complete masked layer.
||| Erases the masked areas.
||| Draw the content layer.
export
present : Area -> Context a -> IO a
present screen (C masked mask content value) = do
  putStr $ fastConcat $ toList masked
  clearMasked screen mask
  putStr $ fastConcat $ toList content
  pure value

||| Apply the given context within the mask layer.
|||
||| This is the internal clipping function which does not first clear
||| the contents of the clipping rectangle.
clip_ : Rect -> Context a -> Context a
clip_ r (C masked mask ops v) = C (masked ++ ops) (r :: mask) [<] v

-------------------------------------------------------------------------------
-- Make Context work with `do` notation
-------------------------------------------------------------------------------

||| Applies `f` to the value.
export
Functor Context where
  map f = {value $= f}

||| Combines the images and mask of both functors.
export
Applicative Context where
  pure x = C [<] [] [<] x
  f <*> (C masked mask content value) = C {
    masked = masked ++ f.masked,
    mask = mask ++ f.mask,
    content = content ++ f.content,
    value = f.value value
  }

||| Merges the images and mask of both functors.
export
Monad Context where
  (C masked mask content value) >>= f =
    let next = f value
    in C {
      masked = masked ++ next.masked,
      mask = mask ++ next.mask,
      content = content ++ next.content,
      value = next.value
    }

||| Debug printf for paint operations
export
debug : Show a => String -> a -> Context a
debug pf x = do
  -- cheat $ ignore $ fPutStrLn stderr $ "\{pf}: \{show x}"
  pure x

||| Move the cursor to the given point
export
moveTo : Pos -> Context ()
moveTo pos = putStr $ cursorMove pos.y pos.x

||| Draw text at the given point
export
showTextAt : Pos -> String -> Context ()
showTextAt pos x = do
  moveTo pos
  putStr x

||| Draw a single character at the given point.
export
showCharAt : Pos -> Char -> Context ()
showCharAt pos x = showTextAt pos (singleton x)

||| Undoes the above
export
unreverseVideo : Context ()
unreverseVideo = putStr "\ESC[27m"

||| This attribute isn't part of the ANSI library in contrib, but is
||| arguably more useful than setting explicit colors.
export
reverseVideo : Context ()
reverseVideo = putStr "\ESC[7m"

||| effectful version for setting arbitrary SGR attributes
export
sgr : List SGR -> Context ()
sgr = putStr . escapeSGR


||| Definitions for box-drawing symbols
namespace Box

  ||| Symbolic type for box drawing characters
  public export
  data Symbol
    = NW
    | NE
    | SW
    | SE
    | H
    | V

  ||| Draw the corresponding box character
  export
  putAt : Pos -> Symbol -> Context ()
  putAt pos NW = showCharAt pos $ cast 0x250C
  putAt pos NE = showCharAt pos $ cast 0x2510
  putAt pos SW = showCharAt pos $ cast 0x2514
  putAt pos SE = showCharAt pos $ cast 0x2518
  putAt pos H  = showCharAt pos $ cast 0x2500
  putAt pos V  = showCharAt pos $ cast 0x2502

  ||| Draw a horizontal line
  export
  hline : Pos -> Nat -> Context ()
  hline pos@(MkPos x y) width = do
    putAt pos H
    case width of
      Z   => pure ()
      S n => hline (MkPos (S x) y) n

  ||| Draw a vertical line
  export
  vline : Pos -> Nat -> Context ()
  vline pos@(MkPos x y) height = do
    putAt pos V
    case height of
      Z   => pure ()
      S n => vline (MkPos x (S y)) n

  ||| Fill a rectangle with the given character
  export
  fill : Char -> Rect -> Context ()
  fill c box = loop box.height box.nw
    where
      loop : Nat -> Pos -> Context ()
      loop Z _ = pure ()
      loop i@(S n) pos = do
        showTextAt pos $ replicate box.width c
        loop n $ pos.shiftDown 1

  ||| Draw a box around the given rectangle
  |||
  ||| Use with `shrink` or `inset` to layout contents within the frame.
  export
  border : Rect -> Context ()
  border r = do
    -- draw the lines at full size
    hline r.nw r.hspan
    hline r.sw r.hspan
    vline r.nw r.vspan
    vline r.ne r.vspan
    -- paint over with the corners
    putAt r.nw NW
    putAt r.ne NE
    putAt r.sw SW
    putAt r.se SE

  ||| Apply the given context within the mask layer.
  |||
  ||| This will:
  ||| - clear the window in the clipping area
  ||| - add the clipping rectangle to the clipping mask
  ||| - paint the original context to the clipping layer
  export
  clip : Rect -> Context a -> Context a
  clip window ctx = clip_ window $ do
    fill ' ' window
    ctx

||| Definitions for unicode arrows.
namespace Arrow

  ||| Symbolic names for different types of arrows
  public export
  data Direction
    = Up
    | Down
    | UpDown
    -- TBD: Left, Right, LeftRight, etc.

  -- TBD: Double-struck, skinny, etc.

  ||| Draw the specified arrow at the current cursor position.
  export
  arrow : Direction -> String
  arrow Up     = "⬆"
  arrow Down   = "⬇"
  arrow UpDown = "⬍"


||| Provides control over terminal modes, like cursor visibility, alt
||| screen mode, and synchronous update mode.
|||
||| As these are only intended to be used from MainLoop
||| implementations, they are `IO` actions, rather than context
||| actions.
namespace VTerm

  ||| Clear the contents of the screen.
  export
  clearScreen : IO ()
  clearScreen = putStr $ eraseScreen All

  ||| Switch into or out of the alternate screen buffer
  export
  altScreen : Bool -> IO ()
  altScreen True  = putStr $ "\ESC[?1049h"
  altScreen False = putStr $ "\ESC[?1049l"

  ||| Show or hide cursor
  export
  cursor : Bool -> IO ()
  cursor True  = putStr "\ESC[?25h"
  cursor False = putStr "\ESC[?25l"

  ||| Tell the terminal to save its state.
  export
  saveCursor : IO ()
  saveCursor = putStr "\ESC7"

  ||| Tell the terminal to restore its state.
  export
  restoreCursor : IO ()
  restoreCursor = putStr "\ESC8"

  ||| synchronous update Supported by iTerm2 and other fancy terminals
  export
  beginSyncUpdate : IO ()
  beginSyncUpdate = putStr "\ESC[?2026h"

  ||| synchronous update supported by iTerm2 and other fancy terminals
  export
  endSyncUpdate : IO ()
  endSyncUpdate = putStr "\ESC[?2026l"
