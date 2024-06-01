||| Minimalist terminal UI framework.
|||
||| Immediate-mode TUI graphics.
|||
||| These routines are slightly higher-level than that provided by
||| `Control.ANSI`. In particular, we use types from `TUI.Geometry`.
|||
||| The goal for the moment is to implement a reasonably-rich set of
||| features, up to and including sixel graphics, for supported
||| terminals. Down the road I might look for graceful fallback
||| options, and the associated mechanism for detecting terminal
||| feature sets terminal. Or else this will all be ported to use
||| ncurses. For now, I'm taking the garden path.

module TUI.Painting


import public Control.ANSI
import Data.String
import System
import public TUI.Geometry


%default total


||| Move the cursor to the given point
export
moveTo : Pos -> IO ()
moveTo pos = putStr $ cursorMove pos.y pos.x

||| Draw text at the given point
export
showTextAt : Pos -> String -> IO ()
showTextAt pos x = do
  moveTo pos
  putStr x

||| Draw a single character at the given point.
export
showCharAt : Pos -> Char -> IO ()
showCharAt pos x = showTextAt pos (singleton x)

||| Undoes the above
export
unreverseVideo : IO ()
unreverseVideo = putStr "\ESC[27m"

||| This attribute isn't part of the ANSI library in contrib, but is
||| arguably more useful than setting explicit colors.
export
reverseVideo : IO ()
reverseVideo = putStr "\ESC[7m"

||| effectful version for setting arbitrary SGR attributes
export
sgr : List SGR -> IO ()
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
  putAt : Pos -> Symbol -> IO ()
  putAt pos NW = showCharAt pos $ cast 0x250C
  putAt pos NE = showCharAt pos $ cast 0x2510
  putAt pos SW = showCharAt pos $ cast 0x2514
  putAt pos SE = showCharAt pos $ cast 0x2518
  putAt pos H  = showCharAt pos $ cast 0x2500
  putAt pos V  = showCharAt pos $ cast 0x2502

  ||| Draw a horizontal line
  export
  hline : Pos -> Nat -> IO ()
  hline pos@(MkPos x y) width = do
    putAt pos H
    case width of
      Z   => pure ()
      S n => hline (MkPos (S x) y) n

  ||| Draw a vertical line
  export
  vline : Pos -> Nat -> IO ()
  vline pos@(MkPos x y) height = do
    putAt pos V
    case height of
      Z   => pure ()
      S n => vline (MkPos x (S y)) n

  ||| Fill a rectangle with the given character
  export
  fill : Char -> Rect -> IO ()
  fill c box = loop box.size.height
    where
      loop : Nat -> IO ()
      loop Z = pure ()
      loop i@(S n) = do
        let pos = MkPos box.pos.x i
        showTextAt pos $ replicate box.size.width c
        loop n

  ||| Draw a box around the given rectangle
  |||
  ||| Use with `shrink` or `inset` to layout contents within the frame.
  export
  box : Rect -> IO ()
  box r = do
    -- draw the lines at full size
    hline r.nw r.size.width
    hline r.sw r.size.width
    vline r.nw r.size.height
    vline r.ne r.size.height
    -- paint over with the corners
    putAt r.nw NW
    putAt r.ne NE
    putAt r.sw SW
    putAt r.se SE


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
