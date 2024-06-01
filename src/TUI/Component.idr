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
||| @state      : The initial application state
||| @view       : The view implementation for our state
||| @controller : The controller implementation for our state.
||| @update     : The global application update function.
public export
interface Component stateT valueT actionT where
  size : stateT -> Area
  paint : State -> Rect -> stateT -> IO ()
  handle : Key -> stateT -> actionT
  update : actionT -> stateT -> Either stateT valueT

||| Default component implementation
|||
||| Component is automatically implemented when Model, View, and
||| Controller are already implemented for `stateT`.
|||
||| You are free to define more specialized implementations.
export
implementation
    Model stateT valueT actionT
 => View stateT
 => Controller stateT actionT
 => Component stateT valueT actionT where
   size = View.size
   paint = View.paint
   handle = Controller.handle
   update = Model.update
