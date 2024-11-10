module Main


import Data.List
import Derive.Prelude
import TUI
import TUI.Geometry
import TUI.MainLoop
import TUI.MainLoop.Default
--import System
--import System.File


%default total
%language ElabReflection

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

||| A simple counter
testCounter : Pos -> Component Custom
testCounter init = component (MkCustom init 0) onKey unavailable
  where
    onKey : Component.Handler Custom Custom Key
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
  case !(runComponent !getDefault (testCounter (!screen).center)) of
    Nothing => putStrLn "User Canceled"
    Just choice => putStrLn $ "User selected: \{show choice}"
