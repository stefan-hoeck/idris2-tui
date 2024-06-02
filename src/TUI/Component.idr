||| Minimalist terminal UI framework.
|||
||| A component combines a view and a controller.
module TUI.Component


import public TUI.Controller
import public TUI.Model
import public TUI.View


%default total


||| Actions common to all components.
|||
||| @ Ignore Don't do anything in response to this event
||| @ Yield  This component has yielded a value.
||| @ Do     Run a component-specific action.
public export
data Response valueT actionT
  = Ignore
  | Yield (Maybe valueT)
  | Do actionT

||| Type alias for the result of component update.
public export
0 Result : Type -> Type -> Type
Result stateT valueT = Either stateT (Maybe valueT)

||| Continue operation with given state.
public export
continue : stateT -> Result stateT _
continue = Left

||| Exit from this component, yielding the given value.
public export
ok : valueT -> Result _ valueT
ok = Right . Just

||| Exit from this component, yielding no value.
public export
cancel : Result _ valueT
cancel = Right Nothing

||| A component combines model, view, and controller.
|||
||| Use this when stateT, valueT and actionT are inter-related.
|||
||| Model, View, and Controller are implemented on Component, so if
||| you implement Component, you need not implement other the
||| interfaces.
public export
interface Component stateT valueT actionT | stateT where
  size   : stateT -> Area
  paint  : State -> Rect -> stateT -> IO ()
  handle : Key -> stateT -> Response valueT actionT
  update : actionT -> stateT -> Either stateT valueT

||| Handle the common actions on Component.
|||
||| This saves us having to specify these cases in every component
||| implementation, and ensures consistent behavior across components.
export
liftUpdate
  :  Component stateT valueT actionT
  => Response valueT actionT
  -> stateT
  -> Result stateT valueT
liftUpdate Ignore      self = Left self
liftUpdate (Yield x)   _    = Right x
liftUpdate (Do action) self = Just <$> Component.update action self

||| Implement model for component.
|||
||| The update function is wrapped by liftUpdate above, so
||| implementations do not need to explicitly handle these cases.
export
implementation
     Component stateT valueT actionT
  => Model stateT (Maybe valueT) (Response valueT actionT)
where
  update response self = liftUpdate response self

||| Implement View for component.
|||
||| We simply delegate to the component methods.
|||
||| XXX: I'm not sure why I had to do it like this, interface
||| resolution seems a bit wonky.
public export
%hint
componentViewImpl
  : {auto impl : Component stateT _ _}
  -> View stateT
componentViewImpl {impl} = MkView Component.size Component.paint

||| Implement Controller for component.
|||
||| We simply delegate to the component method.
export
implementation
     Component stateT valueT actionT
  => Controller stateT (Response valueT actionT)
where
  handle = Component.handle
