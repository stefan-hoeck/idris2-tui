module Main


import Data.List
import Data.List.Quantifiers
import Data.List.Quantifiers.Extra
import Derive.Prelude
import TUI
import TUI.Geometry
import TUI.MainLoop
import TUI.MainLoop.InputShim
import JSON.Derive
--import System
--import System.File


%default total
%language ElabReflection

data CounterEvent = Inc | Reset
%runElab derive "CounterEvent" [Eq, Ord, Show, FromJSON, ToJSON]

record Custom where
  constructor MkCustom
  pos : Pos
  count : Nat
%runElab derive "Custom" [Show]

View Custom where
  size _ = MkArea 1 1
  paint state window self = do
    reverseVideo
    showTextAt self.pos $ show self.count
    sgr [Reset]
    ignore $ packBottom Normal window legend
  where
    legend : String
    legend = "Arrow Keys Move Cursor. <Esc> to Exit."

partial export
main : IO ()
main = do
  window <- screen
  case !(runView !mainLoop handler $ state window.center) of
    Nothing => putStrLn "User Canceled"
    Just choice => putStrLn $ "User selected: \{show choice}"
where
  mainLoop : IO (InputShim [CounterEvent, Key])
  mainLoop = do
    keyboard <- inputShim
    counter  <- raw {eventT = CounterEvent} "Counter"
    pure $ keyboard.addEvent counter

  state : Pos -> Custom
  state pos = MkCustom pos 0

  onCounter : Event.Handler Custom Custom CounterEvent
  onCounter Inc   self = update $ {count $= S} self
  onCounter Reset self = update $ {count $= S} self

  onKey : Event.Handler Custom Custom Key
  onKey Up     self = update $ { pos := self.pos.shiftUp    1} self
  onKey Down   self = update $ { pos := self.pos.shiftDown  1} self
  onKey Left   self = update $ { pos := self.pos.shiftLeft  1} self
  onKey Right  self = update $ { pos := self.pos.shiftRight 1} self
  onKey Enter  self = yield self
  onKey Escape self = exit
  onKey _      self = ignore

  handler : Event.Handler Custom Custom (HSum [CounterEvent, Key])
  handler = union [onCounter, onKey]

