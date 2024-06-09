||| Minimalist terminal UI framework.
|||
||| A controller is a state-machine which interprets user input.
|||
||| For TUI applications, input events mainly consist of keystrokes,
||| though we do not wish to limit ourselves to this notion.
|||
||| The controller receives events from mainloop, producing concrete
||| updates to the application state.
|||
||| As with view, the library supplies a number of generic controllers
||| that can be composed, or your can write your own from scratch.
module TUI.Controller


import Data.Fin
import Data.List
import Data.SortedMap
import public TUI.Event
import public TUI.Model
import Util


%default total


||| Application-supplied response to an input event.
|||
||| @ Ignore Don't do anything in response to this event
||| @ Yield  Yield a value to the parent controller or runtime.
||| @ Do     Update the model via an action.
public export
data Response valueT actionT
  = Ignore
  | Yield (Maybe valueT)
  | Do actionT
  | Run (IO actionT)

||| The result of updating a component
|||
||| This is the
public export
0 Result : Type -> Type -> Type
Result stateT valueT = Either stateT (Maybe valueT)

||| Determine which action to take in response to an input.
public export
interface Controller stateT valueT actionT | stateT where
  ||| Decide what action to take next
  handle : Key -> stateT -> Response valueT actionT

||| Handle common path for controller Response.
|||
||| This saves having to specify these cases in every Model implementation.
export
liftUpdate
  :  Model stateT actionT
  => Response valueT actionT
  -> stateT
  -> IO (Result stateT valueT)
liftUpdate Ignore       self = pure $ Left self
liftUpdate (Yield x)    _    = pure $ Right x
liftUpdate (Do action)  self = pure $ Left $ Model.update action self
liftUpdate (Run effect) self = pure $ Left $ Model.update !effect self

||| Map over the functions action type.
export
Functor (Response valueT) where
  map f Ignore    = Ignore
  map f (Yield x) = Yield x
  map f (Do x)    = Do (f x)
  map f (Run x)   = Run [| f x |]
