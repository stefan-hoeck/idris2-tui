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
import Zipper


%default total


||| A controller's response to user input.
|||
||| @Ignore: Do nothing
||| @Exit:   This controller no longer accepts input.
||| @Update: Update the controller's internal state with a new state.
||| @Do:     Trigger a global action on the next mainloop iteration.
|||
||| state transitions.
public export
data Response stateT actionT
  = Ignore
  | Exit
  | Update stateT
  | Do actionT

public export
interface Controller selfT actionT | selfT where
  ||| Possibly update our state in response to a key press.
  |||
  ||| The default implementation just shifts focus, depending on the
  ||| key-press.
  handle : Key -> selfT -> Response selfT actionT

||| Handle an event for a nested controller.
|||
||| @innerT : The inner controller type
||| @outerT : The outer controller type.
||| @key    : The event to handle.
||| @update : A function to update the parent controller.
export
liftResponse
  :  Controller innerT actionT
  => (key       : Key)
  -> (inner     : innerT)
  -> (update    : innerT -> outerT)
  -> Response outerT actionT
liftResponse key inner f = case handle key inner of
  Ignore          => Ignore
  Exit            => Exit
  Update inner    => Update (f inner)
  Do action       => Do action

||| A function : `Key -> actionT` is implicitly a stateless controller.
export
Controller (Key -> actionT) actionT where
  handle key self = Do $ self key

||| Supports runtime dynamic dispatch to an arbitrary controller.
public export
record Dynamic actionT where
  constructor Dyn
  {auto 0 innerT : Type}
  inner : innerT
  {auto impl : Controller innerT actionT}

||| Implement controller for dynamic
export
Controller (Dynamic actionT) actionT where
  handle key self = liftResponse @{self.impl} key self.inner update
    where
      update : self.innerT -> Dynamic actionT
      update inner = { inner := inner } self

namespace Seq
  ||| Compose controllers by sequencing one after the other.
  |||
  ||| The current controller retains control until it signals that it is
  ||| done via returning `Exit`, at which point we advance to the next
  ||| controller until we reach the end of the list.
  export
  Controller subT actionT => Controller (List subT) actionT where
    handle key [] = Exit
    handle key (x :: xs) = case handle key x of
      Exit           => Update xs
      Ignore         => Ignore
      Update next    => Update $ next :: xs
      Do action      => Do action

||| Dynamic nesting of controllers.
|||
||| A stack is really just a list of dynamic controllers, but treat it
||| as an opaque type.
namespace Stack
  ||| Wrap an inner action with stack actions
  public export
  data Action actionT
    = Push (Dynamic (Action actionT))
    | Pop
    | Lift actionT

  ||| Opaque stack type
  export
  0 Stack : Type -> Type
  Stack actionT = List (Dynamic (Action actionT))

  ||| Construct a new stack with its a concrete root view.
  export
  stack : Controller rootT (Action actionT) => rootT -> Stack actionT
  stack root = [Dyn {innerT = rootT, inner = root}]

  ||| Implement view for stack.
  export
  Controller (Stack actionT) actionT where
    handle key [] = Exit
    handle key (x :: xs) = case handle key x of
      Exit          => Update xs
      Ignore        => Ignore
      Update x      => Update $ x :: xs
      Do (Push y)   => Update $ y :: x :: xs
      Do Pop        => Update xs
      Do (Lift a)   => Do a
