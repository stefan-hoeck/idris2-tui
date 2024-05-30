||| Minimalist terminal UI framework.
|||
||| This module provides dynamic dispatch over views.
module TUI.View.Dynamic

import TUI.View

||| A dynamic dispatch cell for compound views.
|||
||| The concrete type and View implementation is captured
||| implicitly. The impl is kept around for runtime dispatch.
public export
record Dynamic actionT where
  constructor Dyn
  ||| Capture the concrete state type as a parameter
  view           : stateT
  {auto impl     : View stateT actionT}

||| Update the wrapped view with the given value.
export
updateInner
  : (self : Dynamic actionT)
  -> self.stateT
  -> Dynamic actionT
updateInner self inner = { view := inner } self

||| Event handling logic common to all dynamic views.
export
handleDynamic
  : Key
  -> Dynamic actionT
  -> Response (Dynamic actionT) actionT
handleDynamic key self = liftResponse @{self.impl} key self.view (updateInner self)

||| In every respect, a dynamic view is just a view.
export
View (Dynamic actionT) actionT where
  size self  = size @{self.impl} self.view
  paint state window self = paint @{self.impl} state window self.view
  handle = handleDynamic
