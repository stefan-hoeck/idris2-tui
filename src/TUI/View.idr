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


namespace State
  ||| The high-level drawing state for views.
  |||
  ||| This is used by the `paint` method to provide appropriate
  ||| feedback.
  public export
  data State = Normal | Focused | Disabled

  ||| Demote the `Focused` state to the `Normal` state.
  |||
  ||| This is useful for rendering unfocused sibling components within
  ||| a container.
  public export
  demoteFocused : State -> State
  demoteFocused Focused = Normal
  demoteFocused x       = x

  ||| Default styles for rendering text
  export
  styleForState : State -> IO ()
  styleForState Normal   = sgr [Reset]
  styleForState Focused  = reverseVideo
  styleForState Disabled = sgr [SetStyle Faint]

  ||| Paint with the appropriate style for the given state.
  |||
  ||| Completely resets the graphics context after painting.
  export
  withState : State -> IO () -> IO ()
  withState state wrapped = do
    styleForState state
    wrapped
    sgr [Reset]

||| A View can paint itself to the screen in 2D.
|||
||| - It has a drawing state: Focused, Unfocused, or Disabled.
||| - It knows how to size itself, for layout purposes.
||| - It can draw itself to the screen, using a window.
public export
interface View selfT where
  ||| Calculate the "requested" size
  size  : selfT -> Area

  ||| Draw the view into the given screen rectangle.
  paint : State -> Rect -> selfT -> IO ()

||| Implement `View` for `()` as a no-op
export
View () where
  size  _     = MkArea 0 0
  paint _ _ _ = pure ()

||| Any type implementing `Show` can be painted.
export
Show a => View a where
  size s = MkArea (length (show s)) 1
  paint state r s = withState state $ showTextAt r.nw (show s)

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
