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


||| Determine which action to take in response to input.
public export
interface Controller selfT actionT where
  ||| Decide what action to take next
  handle : Key -> selfT -> actionT

||| A function : `Key -> actionT` is implicitly a stateless controller.
export
Controller (Key -> actionT) actionT where
  handle key self = self key

||| A dynamic dispatch cell for an arbitrary controller.
public export
record Dynamic actionT where
  constructor Dyn
  {auto 0 innerT : Type}
  inner : innerT
  {auto impl : Controller innerT actionT}

||| Implement controller for dynamic
export
Controller (Dynamic actionT) actionT where
  handle key self = handle @{self.impl} key self.inner
