||| Minimalist terminal UI framework.
|||
||| See README.md for a detailed explanation of the code in this
||| package.
module TUI

import public TUI.Component
import public TUI.Component.Editor
import public TUI.Component.Form
import public TUI.Component.Menu
import public TUI.Component.Numeric
import public TUI.Component.Stack
import public TUI.Component.Table
import public TUI.Component.TextInput
import public TUI.Component.VList
import public TUI.Controller
import public TUI.Event
import public TUI.Image
import public TUI.Layout
import public TUI.MainLoop
import public TUI.Painting
import public TUI.View


%default total
%language ElabReflection


||| A simple menu, useful for testing.
export
testMenu : Exclusive String
testMenu = menu ["foo", "bar", "baz"]

{-
export
testDynamic : Dynamic ()
testDynamic = Dyn testMenu

export
testField : Field ()
testField = field "Test Field" testMenu

record Test where
  constructor MkTest
  container : Container (Field ())

test : Test
test = MkTest $ fromList [testField]

View Test () where
  size self = sizeVertical $ Zipper.List.toList self.container
  paint state window self = paintVertical @{splitAt $ length "Test Field"} state window $ self.container
  handle key self = liftResponse key update self.container
    where
      update : Container (Field ()) -> Test
      update container = { container := container } self

||| A simple form, useful for testing.
export
testForm : Form ()
testForm = form [
  field "F1"         testMenu,
  field "Long name"  testMenu,
  field "Text Input" $ TextInput.fromString "test",
  field "Test"       $ TextInput.fromString "test"
]

-}

partial export
gallery : IO ()
gallery = do
  let result : Maybe String = !(runMVC [] testMenu)
  case result of
    Nothing => putStrLn "User Canceled"
    Just choice => putStrLn $ "User selected: \{show choice}"

{-
||| Demonstrate all the widgets, as they are implemented.
|||
||| Useful for smoke-testing changes to the library.
partial export
gallery : IO ()
gallery = do
  v <- runView (const pure) [] $ form [
    field "menu"    testMenu,
    field "Nat"     $ numeric (the Nat 5)        1 (),
    field "Integer" $ numeric (the Integer 5)    1 (),
    field "Double"  $ numeric (the Double 5.0) 0.1 (),
    field "nested"  testForm
  ]
  putStrLn ""
