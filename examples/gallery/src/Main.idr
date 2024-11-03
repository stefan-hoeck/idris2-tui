module Main


import Data.List
import TUI
import TUI.MainLoop
import TUI.MainLoop.Default
import System
import System.File

%default total
%language ElabReflection


||| A simple counter
testCounter : Component Nat
testCounter = component @{show} 0 onKey unavailable
  where
    onKey : Component.Handler Nat Nat Key
    onKey Up    cur = update $ cur + 1
    onKey Down  cur = update $ cur `minus` 1
    onKey (Alpha 'i') cur = update $ cur + 1
    onKey (Alpha 'd') cur = update $ cur `minus` 1
    onKey Enter cur = yield cur
    onKey Escape cur = exit
    onKey _     _   = ignore

||| A simple menu
testMenu : Component String
testMenu = Spinner.fromChoice ["foo", "bar", "baz"] "bar"

||| A component that represents a user-chosen value
record TestModal where
  constructor TM
  label     : String
  selection : Maybe String

(.helper) : TestModal -> String
(.helper) self = "Selection: \{fromMaybe "" self.selection}"

||| Implement show for the component (and thereby View)
View TestModal where
  size self = hunion (size @{show} self.label) (size self.helper)
  paint state window self = do
    window <- packTop (demoteFocused state) window self.label
    ignore $  packTop state window self.helper

||| Construct a TestModal component
testModal2 : Component String
testModal2 = component (TM header Nothing) onKey unavailable
  where
    header : String
    header = "Modal 2: (a): Baz, (b): Quux, (c): From Spinner"

    onSelect : Maybe String -> TestModal -> TestModal
    onSelect selection = {selection := selection}

    onKey : Component.Handler TestModal String Key
    onKey (Alpha 'a') _  = yield "Baz"
    onKey (Alpha 'b') _  = yield "Quux"
    onKey (Alpha 'c') s  = push testMenu ((flip onSelect) s)
    onKey Enter       s  = exitIf s.selection
    onKey Escape      _  = exit
    onKey _           _  = ignore

testModal1 : Component String
testModal1 = component (TM header Nothing) onKey unavailable
  where
    header : String
    header = "Modal 1: (a): Foo, (b): Bar, (c): From Modal 2"

    onSelect : Maybe String -> TestModal -> TestModal
    onSelect selection = {selection := selection}

    onKey : Component.Handler TestModal String Key
    onKey (Alpha 'a') _ = yield "Foo"
    onKey (Alpha 'b') _ = yield "Bar"
    onKey (Alpha 'c') s = push testModal2 ((flip onSelect) s)
    onKey Enter       s = exitIf s.selection
    onKey Escape      _ = exit
    onKey _           _ = ignore

testForm : Component (HVect [String, Nat, Integer, Double, String])
testForm = ariaForm [
  F     "menu"    testMenu,
  F     "Nat"     $ numeric (the Nat 5),
  F     "Integer" $ numeric (the Integer 5),
  field "Double"  $ Just 5.0,
  F     "String"  $ pushButton "TestModal1" testModal1
]

partial export
gallery : IO ()
gallery = do
  case !(runComponent @{inset} !getDefault testForm) of
    Nothing => putStrLn "User Canceled"
    Just choice => putStrLn $ "User selected: \{show choice}"

||| Application entry point
partial
main : IO ()
main = gallery
