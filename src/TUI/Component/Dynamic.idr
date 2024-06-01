||| Minimalist terminal UI framework.
|||
||| This module provides dynamic dispatch over views.
module TUI.Component.Dynamic

import TUI.Component

||| A dynamic dispatch cell for compound views.
|||
||| The concrete type and View implementation is captured
||| implicitly. The impl is kept around for runtime dispatch.
public export
record Dynamic valueT actionT where
  constructor Dyn
  ||| Capture the concrete state type as a parameter
  inner          : stateT
  {auto impl     : Component stateT valueT actionT}

||| Proxy to the wrapped type.
export
Component (Dynamic valueT actionT) valueT actionT where
  size   self               = size   @{self.impl} self.inner
  paint  state  window self = paint  @{self.impl} state window self.inner
  handle key    self        = handle @{self.impl} key self.inner
  update action self        = case update @{self.impl} action self.inner of
    Left inner => Left $ { inner := inner } self
    Right value => Right value
