||| Minimalist terminal UI framework.
|||
||| A View is an interactive interface component. It knows how to
||| paint itself, and it knows how to update itself in response to
||| user input. It also can send signals upstream to the application.
|||
||| XXX: Should it be renamed to `Widget` or something similar?
module TUI.View


import Data.Fin
import public TUI.Event
import public TUI.Painting
import Data.List1
import Data.SortedMap
import Zipper


||| A response to an input event.
|||
||| This is returned by the `handle` method, and covers the possible
||| actions supported by the framework.
|||
||| Exit      : Subview wants to give up focus.
||| Update    : The event is handled, yielding a new state.
||| Do action : An action to be run in the top-level handler.
public export
data Response stateT actionT
  = Exit
  | Update stateT
  | Do     actionT

||| A view is a high-level UI component.
|||
||| - It wraps an inner value, its state.
||| - It knows how to size itself, for layout purposes.
||| - It can draw itself to the screen
||| - It can update its state in response to events.
public export
interface View stateT actionT | stateT where
  constructor MkView
  ||| Calculate the "requested" size
  size  : stateT -> Area

  ||| Draw the view into the given screen rectangle.
  paint : State -> Rect -> stateT -> IO ()

  ||| Possibly update our state in response to a key press.
  |||
  ||| The default implementation just shifts focus, depending on the
  ||| key-press.
  handle : Key -> stateT -> Response stateT actionT
  handle _ self = Update self

||| Implement `View` for `()` as a no-op
export
View () Void where
  size  _     = MkArea 0 0
  paint _ _ _ = pure ()

||| Any type implementing `Show` is automatically a (non-interative)
||| view.
export
Show a => View a actionT where
  size s = MkArea (length (show s)) 1
  paint state r s = withState state $ showTextAt r.nw (show s)

||| In implementing `View` for all `Show` types, we have
||| inadvertently made it ambigious what to do when we use a string
||| as a view. This alternative, named implementation draws the
||| string directly to the screen.
export
[string] View String actionT where
  size s = MkArea (length s) 1
  paint state r self = withState state $ showTextAt r.nw self

||| Size the given list of views, laying them out vertically within.
export
sizeVertical : View itemT _ => List itemT -> Area
sizeVertical self = foldl (flip $ hunion . size) (MkArea 0 0) self

||| Paint the given view into the top of the given window.
|||
||| Return the remaning space in the window.
export
packTop : View v actionT => State -> Rect -> v -> IO Rect
packTop state window self = do
  let split = (size self).height
  let (top, bottom) = vsplit window split
  paint state top self
  pure bottom

||| Paint the given list of views, laying them out vertically within
||| `window`.
export
paintVertical
  : View v _
  => (state   : State)
  -> (window  : Rect)
  -> (self    : List v)
  -> IO Rect
paintVertical state window [] = pure window
paintVertical state window (x :: xs) = paintVertical state !(packTop state window x) xs

||| Handle an event for a nested view.
|||
||| @innerT : The nested view type
||| @outerT : The containing view
||| @key    : The event to handle
||| @update : A function to update the parent view.
export
liftResponse
  :  View innerT actionT
  => (key       : Key)
  -> (inner     : innerT)
  -> (update    : innerT -> outerT)
  -> Response outerT actionT
liftResponse key inner f = case handle key inner of
  Exit         => Exit
  Update inner => Update (f inner)
  Do action    => Do action

||| Implement View for `maybe` of any view.
|||
||| If `Just`, then we paint the given subview. Otherwise it behaves
||| as if it were not present.
export
View v a => View (Maybe v) a where
  size (Just self) = size self
  size Nothing     = MkArea 0 0

  paint state window (Just self) = paint state window self
  paint _    _       Nothing     = pure ()

  handle key (Just self) = liftResponse key self Just
  handle key Nothing = Update Nothing

||| Implement view for Either
export
View v1 a => View v2 a => View (Either v1 v2) a where
  size (Left self)  = size self
  size (Right self) = size self

  paint state window (Left self)  = paint state window self
  paint state window (Right self) = paint state window self

  handle key (Left  self) = liftResponse key self Left
  handle key (Right self) = liftResponse key self Right

||| A concrete view type for horizontal and vertical separators.
public export
data Rule = HRule | VRule

||| View implementation for rules
export
View Rule _ where
  size HRule = MkArea 0 1
  size VRule = MkArea 1 0

  paint _ window HRule = hline window.nw window.size.width
  paint _ window VRule = vline window.nw window.size.height

||| Paint two views into the given rectangle with a vertical line
||| between them.
export
hpane
  : View leftT actionT
  => View rightT actionT
  => State
  -> Rect
  -> leftT
  -> rightT
  -> Nat
  -> IO ()
hpane state window left right split = do
  let (l_window, r_window) = hdivide window split
  paint state l_window left
  vline (MkPos split window.n) window.size.height
  paint state r_window right
