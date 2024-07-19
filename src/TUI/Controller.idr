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
import Util


%default total


||| A function which yields the next state.
public export
0 Update : Type -> Type
Update stateT = stateT

||| A function which yields an IO action of the next state.
public export
0 Effect : Type -> Type
Effect stateT = IO stateT

||| Application-supplied response to an input event.
|||
||| @ Ignore Don't do anything in response to this event
||| @ Yield  Yield a value to the parent controller or runtime.
||| @ Do     Update the model via an action.
||| @ Run    Perform an IO action on the model.
public export
data Response stateT valueT
  = Ignore
  | Yield (Maybe  valueT)
  | Do    (Update stateT)
  | Run   (Effect stateT)

||| The result of updating a component
public export
0 Result : Type -> Type -> Type
Result stateT valueT = Either stateT (Maybe valueT)

||| Determine which action to take in response to an input.
public export
interface Controller stateT valueT | stateT where
  ||| Decide what action to take next
  handle : Key -> stateT -> Response stateT valueT

||| Handle common path for controller Response.
|||
||| This saves having to specify these cases in every Model implementation.
export
liftUpdate
  :  Response stateT valueT
  -> stateT
  -> IO (Result stateT valueT)
liftUpdate Ignore     self = pure $ Left self
liftUpdate (Yield x)  _    = pure $ Right x
liftUpdate (Do next)  _    = pure $ Left next
liftUpdate (Run next) _    = pure $ Left !next
