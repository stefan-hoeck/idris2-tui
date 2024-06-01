||| Minimalist terminal UI framework.
|||
||| A component combines a view and a controller.
module TUI.Component


import public TUI.Controller
import public TUI.Model
import public TUI.View


%default total

||| Extend an inner action with these common patterns.
public export
data Response valueT actionT
  = Ignore
  | Yield valueT
  | Do actionT

||| A component combines model, view, and controller.
|||
||| There's a particular relationship between these interfaces that we
||| try to capture here.
public export
interface Component stateT valueT actionT | stateT where
  size   : stateT -> Area
  paint  : State -> Rect -> stateT -> IO ()
  handle : Key -> stateT -> Response valueT actionT
  update : Response valueT actionT -> stateT -> Either stateT valueT

||| Lift an event source to work with our component
export
liftSource
  :  Component stateT valueT actionT
  => EventSource stateT actionT
  -> EventSource stateT (Response valueT actionT)
liftSource source = Do <$> source

||| Handle updates on a wrapped Response
export
liftUpdate
  :  Model stateT valueT actionT
  => Response valueT actionT
  -> stateT
  -> Either stateT valueT
liftUpdate Ignore      self = Left self
liftUpdate (Yield x)   _    = Right x
liftUpdate (Do action) self = update action self

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
  => Controller stateT (Response valueT actionT)
  => Component  stateT valueT actionT
where
   size = View.size
   paint = View.paint
   handle = Controller.handle
   update = liftUpdate
