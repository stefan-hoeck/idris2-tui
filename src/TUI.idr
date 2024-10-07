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
testMenu : Component String
testMenu = Menu.component ["foo", "bar", "baz"]

||| Test modal components
export
testModal : Component String
testModal = root (static "(a): Foo, (b): bar, (c): push") handle
  where
    handle : Key -> Modal String -> Response (Modal String) String
    handle (Alpha 'a') _ = Yield $ Just "Foo"
    handle (Alpha 'b') _ = Yield $ Just "Bar"
    handle (Alpha 'c') s = Do $ push testMenu s merge_
      where
        merge_ : String -> Component s.topT -> Component s.topT
        merge_ string top = ?hole
    handle _           _ = Ignore


partial export
gallery : IO ()
gallery = do
  let result : Maybe String = !(runComponent [] testModal)
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
