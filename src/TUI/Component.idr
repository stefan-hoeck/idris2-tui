||| Minimalist terminal UI framework.
|||
||| A component combines a view and a controller.
module TUI.Component


import public TUI.Controller
import public TUI.View
import public TUI.Event


%default total


public export
record Component valueT where
  constructor MkComponent
  0 State : Type
  state   : State
  handler : Key -> State -> Response State valueT
  vimpl   : View State

export
View (Component _) where
  size self = size @{self.vimpl} self.state
  paint state window self = paint @{self.vimpl} state window self.state

export
Controller (Component valueT) valueT where
  handle key self = case self.handler key self.state of
    Ignore  => Ignore
    Yield v => Yield v
    Do  s   => Do $ {state := s} self
    Run e   => Run $ do pure $ {state := !e} self

export
component
  : View stateT
  => Controller stateT valueT
  => stateT
  -> Component valueT
component init = MkComponent {
  State = stateT,
  state = init,
  handler = handle,
  vimpl = %search
}

export
withHandler
  : View stateT
  => stateT
  -> (Key -> stateT -> Response stateT valueT)
  -> Component valueT
withHandler init handler = MkComponent {
  State = stateT,
  state = init,
  handler = handler,
  vimpl = %search
}
