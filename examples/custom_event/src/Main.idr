module Main


import Derive.Prelude
import TUI
import TUI.MainLoop
import TUI.MainLoop.InputShim
import JSON.Derive


%default total
%language ElabReflection


||| This is the user-defined event type.
|||
||| To satisfy the requirements of `InputShim`, it must implement
||| `FromJSON`.
data Counter
  = Inc
  | Reset
%runElab derive "Counter" [FromJSON]

||| The demo state consists of a cursor position and a count.
|||
||| The cursor position is updated via the keyboard, as usual. The
||| count is updated via the user timer event defined above.
record UserEventDemo where
  constructor MkUser
  pos : Pos
  count : Nat
%runElab derive "UserEventDemo" [Show]

||| We implement view for this type as per usual.
|||
||| Basically, we show the count at the current cursor position.
View UserEventDemo where
  size _ = MkArea 1 1
  paint state window self = do
    reverseVideo
    showTextAt self.pos $ show self.count
    sgr [Reset]
    ignore $ packBottom Normal window legend
  where
    legend : String
    legend = "Arrow Keys Move Cursor. <Esc> to Exit."

||| This shows how to create a component which responds to a user event.
|||
||| This uses `component'` instead of `component`.
userEventDemo : Pos -> Component (HSum [Counter, Key]) UserEventDemo
userEventDemo pos = component' {
  state = (MkUser pos 0),
  handler = union [onCounter, onKey],
  get = Just . id
} where
  ||| EventHandler is just like `Component.Handler`, except that it
  ||| needs to be aware of the full set of events that we handle
  onCounter : EventHandler [Counter, Key] UserEventDemo UserEventDemo Counter
  onCounter Inc   self = update $ {count $= S} self
  onCounter Reset self = update $ {count $= S} self

  onKey : EventHandler [Counter, Key] UserEventDemo UserEventDemo Key
  onKey Up     self = update $ { pos := self.pos.shiftUp    1} self
  onKey Down   self = update $ { pos := self.pos.shiftDown  1} self
  onKey Left   self = update $ { pos := self.pos.shiftLeft  1} self
  onKey Right  self = update $ { pos := self.pos.shiftRight 1} self
  onKey Enter  self = yield self
  onKey Escape self = exit
  onKey _      self = ignore

partial export
main : IO ()
main = do
  window   <- screen
  -- start with the basic event loop
  keyboard <- inputShim
  -- then append our custom event source to it.
  counter  <- raw {eventT = Counter} "Counter"
  let mainLoop = keyboard.addEvent counter
  -- now we can run as we would any stock component.
  case !(runComponent mainLoop $ userEventDemo window.center) of
    Nothing => putStrLn "User Canceled"
    Just choice => putStrLn $ "User selected: \{show choice}"
