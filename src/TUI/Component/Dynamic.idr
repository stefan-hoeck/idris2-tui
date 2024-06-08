||| Minimalist terminal UI framework.
|||
||| This module provides dynamic dispatch over views.
module TUI.Component.Dynamic

import TUI.Component

||| A dynamic dispatch cell for components.
|||
||| The concrete type and View implementation is captured
||| implicitly. The impl is kept around for runtime dispatch.
public export
record Dynamic valueT actionT where
  constructor Dyn
  ||| Capture the concrete state type as a parameter
  inner          : stateT
  {auto impl     : Component stateT valueT actionT}

Model (Dynamic valueT actionT) actionT where
  update action self = {inner $= Model.update @{model} action} self

View (Dynamic valueT actionT) where
  size  self              = size  @{view @{self.impl}} self.inner
  paint state window self = paint @{view @{self.impl}} state window self.inner

||| Proxy to the wrapped type.
export
Controller (Dynamic valueT actionT) valueT actionT where
  handle key self = handle @{controller @{self.impl}} key self.inner
