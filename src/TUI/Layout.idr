||| Minimalist terminal UI framework.
|||
||| This module contains code for arranging views on the screen.
module TUI.Layout

import TUI.Painting
import TUI.Geometry
import TUI.View


%default total


||| Size the given list of views, laying them out vertically within.
export
sizeVertical : View itemT => List itemT -> Area
sizeVertical self = foldl (flip $ hunion . size) (MkArea 0 0) self

||| Paint the given view into the top of the given window.
|||
||| Return the remaning space in the window.
export
packTop : View v => State -> Rect -> v -> Context Rect
packTop state window self = do
  let split = (size self).height
  let (top, bottom) = vsplit window split
  paint state top self
  pure bottom

||| Paint the given list of views, laying them out vertically within
||| `window`.
export
paintVertical
  : View v
  => (state   : State)
  -> (window  : Rect)
  -> (self    : List v)
  -> Context Rect
paintVertical state window [] = pure window
paintVertical state window (x :: xs) = paintVertical state !(packTop state window x) xs

||| Size the given list of views, laying them out horizontally within.
export
sizeHorizontal : View itemT => List itemT -> Area
sizeHorizontal self = foldl (flip $ vunion . size) (MkArea 0 0) self

||| Paint the given view into the left of the given window.
|||
||| Return the remaning space in the window.
export
packLeft : View v => State -> Rect -> v -> Context Rect
packLeft state window self = do
  let split = (size self).width
  let (left, right) = hdivide window split
  paint state left self
  pure right

||| Paint the given list of views, laying them out vertically within
||| `window`.
export
paintHorizontal
  : View v
  => (state   : State)
  -> (window  : Rect)
  -> (self    : List v)
  -> Context Rect
paintHorizontal state window [] = pure window
paintHorizontal state window (x :: xs) = paintHorizontal state !(packLeft state window x) xs

||| A concrete view type for horizontal and vertical separators.
public export
data Rule = HRule | VRule

||| View implementation for rules
export
View Rule where
  size HRule = MkArea 0 1
  size VRule = MkArea 1 0

  paint _ window HRule = hline window.nw window.size.width
  paint _ window VRule = vline window.nw window.size.height

||| Paint two views into the given rectangle with a vertical line
||| between them.
export
hpane
  : View leftT
  => View rightT
  => State
  -> Rect
  -> leftT
  -> rightT
  -> Nat
  -> Context ()
hpane state window left right split = do
  let (l_window, r_window) = hdivide window split
  paint state l_window left
  vline (MkPos split window.n) window.size.height
  paint state r_window right
