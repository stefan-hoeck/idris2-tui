||| Minimalist terminal UI framework.
|||
||| A component combines a view and a controller.
module TUI.Component


import public TUI.View
import public TUI.Controller


%default total


||| A component combines a view and a controller in a single value.
export
record Component stateT actionT where
  constructor MkComponent
  state      : stateT
  view       : View stateT
  controller : Controller stateT actionT

||| A component is automatically a view for itself.
export
View (Component stateT _) where
  size self = size @{self.view} self.state
  paint state window self = paint @{self.view} state window self.state

||| A component is automatically a controller for itself.
export
Controller (Component stateT actionT) actionT where
  handle key self = liftResponse @{self.controller} key self.state update
    where
      update : stateT -> Component stateT actionT
      update next = { state := next } self
