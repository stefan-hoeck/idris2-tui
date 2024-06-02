||| Minimalist terminal UI framework.
|||
||| List of known supported terminals:
||| - iTerm2
||| - foot
||| - kitty
||| - alacritty
|||
||| List of unsupported terminals:
||| - intelliJ (not tested)
||| - tmux (no sync update, to investigate)
||| - VTE terminals (not tested)
||| - anything not mentioned above.
|||
||| In addition, for sixel image support, there's a soft dependency on
||| `chafa` (an apt name) which I have no way to enforce. Distro
||| maintainers take note.
|||
||| In comparison to ncurses, this library does direct drawing to the
||| terminal. And so, there are no intermediate buffers. Moreover,
||| there is no optimization of screen updates.
|||
||| Instead of optimizing updates, I use [Kitty's synchronous update
||| protocol](tbd). This is merely ignored by unsupported terminals,
||| so everything will still work, but you may notice flicker. If this
||| bothers you, I appologize. Bear with me for the moment, or consider
||| trying one of the supported terminals.
|||
||| For the moment, I don't want to implement buffers and differential
||| updates:
||| - It's not clear to me how that plays with sixel graphics
||| - I'm not sure exactly what ANSI-like features I want to support
||| - I don't want to go down that rabbit-hole just yet.
||| - Ideally, such a heavy-weight solution would be opt-in.
|||
||| What you lose with this approach is direct support for overlapping
||| windows. You can implement them by maintaining position and
||| drawing order yourself, but you need to *manually* erase any
||| underlying content you don't want showing through. `fill` is
||| provided for this purpose.
|||
||| More tragically, there is no support for *clipping*. It's up to
||| you to ensure you don't render out-of-bounds.
|||
||| And so, the drawing model favors a left-to-right, top-to-bottom
||| drawing order, that is robust against the inability to clip
||| updates to a region. Fortunately this dovetails with structural
||| recursion. See `*split`, and `*divide` in `TUI.Geometry`
||| module. Or see `pack*` and `paint*` in `TUI.View`.
||| I can't get ncurses-idris working, so I'm rolling this pure-idris
||| alternative.
|||
||| I plan to support this standard for [unambigous keyboard
||| input](here). For now, if you want to send a literal escape, you
||| must press escape twice.  I would welcome help implementing this,
||| but I'll get to it eventually.
|||
||| Other limitations include:
||| - no support for termcap or terminfo,
|||   - and so, no facilities for graceful degredation.
|||
||| The primary advantage is sheer simplicity: no dependencies are
||| required beyond `contrib`.
|||
||| This module is soon to become a stand-alone project.
module TUI

import public TUI.Component
import public TUI.Component.Container
import public TUI.Component.Dynamic
import public TUI.Component.Form
import public TUI.Component.Menu
import public TUI.Component.Numeric
import public TUI.Component.Table
import public TUI.Component.TextInput
-- import public TUI.Component.Popup
-- import public TUI.Component.SOP
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
  let result : Maybe String = !(runComponent [] updateOnly testMenu)
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
