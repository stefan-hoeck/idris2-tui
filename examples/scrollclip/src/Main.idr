module Main


import TUI
import TUI.Component.Box
import TUI.MainLoop
import TUI.MainLoop.Default


%default total


-------------------------------------------------------------------------------
-- Erasing around a rectangle:
--
-- +-----------------------------+
-- |                             |
-- |            n                |
-- |     +-------------+         |
-- |     |             |         | Original window
-- |   w |   clip      | e       |
-- |     |             |         |
-- |     +-------------+         |
-- |            s                |
-- +-----------------------------+
--
-- The clip region divides the rect into four rectangles:
--
-- +-----------------------------+
-- |     |             |         |
-- |     |      t      |         |
-- |---- +-------------+---------|
-- |     |             |         | Clipping rectangles
-- |   l |   clip      | r       |
-- |     |             |         |
-- |-----+-------------+---------|
-- |     |      b      |         |
-- +-----------------------------+
--
-- We first paint the content to be clipped. Then we clear any of the
-- four rects around it that are nonempty.
--
-- This is hugely inefficient for all the line-by-line drawing, and
-- over-drawing. But it should be correct. And we can use it to test
-- against other, possibly faster algorithims.
-------------------------------------------------------------------------------


eraseAround
  :  (clip : Rect)
  -> Context ()
eraseAround clip = do
  -- xxx: we need to know the *real* screen width.
  -- this should be available in the Context monad.
  window <- cheat screen
  -- bail early if clip not contained in screen.
  True <- pure $ window.contains clip
       | False => pure ()
  let (t, _) = window.splitTop  clip.n
  let (_, b) = window.splitTop  (clip.s + 1)
  let (l, _) = window.splitLeft clip.w
  let (_, r) = window.splitLeft (clip.e + 1)
  fill ' ' t
  fill ' ' b
  fill ' ' l
  fill ' ' r

items : VList String
items = fromList "Items" $ item <$> [0..20]
  where
    item : Nat -> String
    item i = "This is item: \{show i}"

record ViewPort a where
  constructor MkViewPort
  contents : a
  offset   : (Integer, Integer)

View a => View (ViewPort a) where
  size self = MkArea 0 0
  paint state window self = do
    -- setLayer Scroll
    paint state (offset window self.offset) self.contents
    eraseAround window
    showTextAt (MkPos 0 81) $ show (offset window self.offset)
    -- setLayer Default
  where
    offset : Rect -> (Integer, Integer) -> Rect
    offset self (x, y) = case (x < 0, y < 0) of
      (True, True)   => (self.shiftLeft  $ cast $ -x).shiftUp   $ cast $ -y
      (True, False)  => (self.shiftLeft  $ cast $ -x).shiftDown $ cast    y
      (False, True)  => (self.shiftRight $ cast    x).shiftUp   $ cast $ -y
      (False, False) => (self.shiftRight $ cast    x).shiftDown $ cast    y

[mine] View (Integer, Integer) where
  size = const $ MkArea 0 0
  paint state window self = do
    paint state (MkRect (MkPos 5 5) (MkArea 25 5)) viewport
    window <- packTop state window $ box (MkArea 0 5) 'x'
    ignore $ packLeft state window $ box (MkArea 5 0) 'o'
    showTextAt origin $ show self
  where
    viewport : ViewPort (VList String)
    viewport = MkViewPort items self

-- key idea: ensure scrolled contents never visible outside scrolled area.
--
-- add "layers" to the context, so you can slip draw ops into the
-- context in a different draw order.
--
-- on bottom layer:
--  - paint full contents to be scrolled, including sixels, at correct offset.
--  - erase around viewport window
-- on top layer:
--  - draw all other chrome.


-- this experiment proves that we need to handle negative coordinates
-- gracefully.

main : IO ()
main = do
  _ <- runView @{mine} !getDefault onKey (0, 0)
  pure ()
where
  onKey : Event.Handler (Integer, Integer) () Key
  onKey Up   (x, y) = update $ (x, y - 1)
  onKey Down (x, y) = update $ (x, y + 1)
  onKey Left (x, y) = update $ (x - 1, y)
  onKey Right(x, y) = update $ (x + 1, y)
  onKey _ _ = exit
