||| Minimalist terminal UI framework.
|||
||| I can't get ncurses-idris working, so I'm rolling this pure-idris
||| alternative.
|||
||| It's higher-level than ncurses, with the goal of being able to
||| quickly create keyboard-driven interfaces that are 'good enough'
||| for experimentation and prototyping, with an eye toward efficient
||| data entry.
|||
||| The lack of ncurses support does pose some challenges. In
||| particular, it's not possible to distinguish between the start of
||| an escape sequence or the user pressing the escape key, as there
||| is no non-blocking way to get the next character from stdin.
|||
||| Other limitations include:
||| - no support for termcap or terminfo,
|||   - no checking or fallback.
|||
||| The primary advantage is sheer simplicity: no dependencies are
||| required beyond `contrib`, so long as you rely on an
||| ANSI-compatible terminal or emulator.
|||
||| Anyway, the goal is to eventually promote this module to a
||| separate project, once it's good enough for AMPII. Try to keep the
||| ampii-specific code out of this module.
module TUI

import public TUI.Painting
import public TUI.MainLoop
import public TUI.View

import public TUI.View.Container
import public TUI.View.Dynamic
import public TUI.View.Form
import public TUI.View.Menu
import public TUI.View.Numeric
import public TUI.View.Table
import public TUI.View.TextInput
-- import public TUI.View.Scrollable
-- import public TUI.View.Popup
-- import public TUI.View.Toggle
-- import public TUI.View.Button
-- import public TUI.Layout
-- import public TUI.View.SOP
-- import public TUI.View.Split
-- import public TUI.View.Sequence


%default total
%language ElabReflection


||| A simple menu, useful for testing.
export
testMenu : Menu String ()
testMenu = menu () ["foo", "bar", "baz"]

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

partial export
gallery : IO ()
-- gallery = ignore $ runView (const pure) [] TUI.test
gallery = ignore $ runView (const pure) [] testForm

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
