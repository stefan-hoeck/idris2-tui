||| Minimalist terminal UI framework.
|||
||| A component combines a view and a controller.
module TUI.Component


import public TUI.Controller
import public TUI.Model
import public TUI.View


%default total


||| A component combines model, view, and controller.
|||
||| Use this when stateT, valueT and actionT are inter-related.
|||
||| Model, View, and Controller are implemented on Component, so if
||| you implement Component, you need not implement other the
||| interfaces.
public export
interface
     View stateT
  => Model stateT actionT
  => Controller stateT valueT actionT
  => Component stateT valueT actionT | stateT
where
  model : Model stateT actionT
  model = %search

  view : View stateT
  view = %search

  controller : Controller stateT valueT actionT
  controller = %search
