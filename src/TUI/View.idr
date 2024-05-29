||| Minimalist terminal UI framework.
|||
||| A View is an interactive interface component. It knows how to
||| paint itself, and it knows how to update itself in response to
||| user input. It also can send signals upstream to the application.
|||
||| XXX: Should it be renamed to `Widget` or something similar?
module TUI.View


import public TUI.Painting


%default total


||| A view is a high-level UI component.
|||
||| - It wraps an inner value, its state.
||| - It knows how to size itself, for layout purposes.
||| - It can draw itself to the screen
||| - It can update its state in response to events.
public export
interface View stateT where
  constructor MkView
  ||| Calculate the "requested" size
  size  : stateT -> Area

  ||| Draw the view into the given screen rectangle.
  paint : State -> Rect -> stateT -> IO ()

||| Implement `View` for `()` as a no-op
export
View () where
  size  _     = MkArea 0 0
  paint _ _ _ = pure ()

||| Any type implementing `Show` is automatically a (non-interative)
||| view.
export
Show a => View a where
  size s = MkArea (length (show s)) 1
  paint state r s = withState state $ showTextAt r.nw (show s)

||| In implementing `View` for all `Show` types, we have
||| inadvertently made it ambigious what to do when we use a string
||| as a view. This alternative, named implementation draws the
||| string directly to the screen.
export
[string] View String where
  size s = MkArea (length s) 1
  paint state r self = withState state $ showTextAt r.nw self

||| Implement View for `maybe` of any view.
|||
||| If `Just`, then we paint the given subview. Otherwise it behaves
||| as if it were not present.
export
View v => View (Maybe v) where
  size (Just self) = size self
  size Nothing     = MkArea 0 0

  paint state window (Just self) = paint state window self
  paint _    _       Nothing     = pure ()

||| Implement view for Either.
|||
||| Simply delegates to whichever variant is present.
export
View v1 => View v2 => View (Either v1 v2) where
  size (Left self)  = size self
  size (Right self) = size self

  paint state window (Left self)  = paint state window self
  paint state window (Right self) = paint state window self
