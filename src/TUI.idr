-- BSD 3-Clause License
--
-- Copyright (c) 2023, Brandon Lewis
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

||| TUI Framework.
|||
||| See README.md for a detailed explanation of the code in this
||| package.
module TUI

import public TUI.Component
import public TUI.Component.Editor
import public TUI.Component.Form
import public TUI.Component.Menu
import public TUI.Component.Numeric
import public TUI.Component.PushButton
import public TUI.Component.Modal
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


-- The rest of the code in this file is a test suite. But this stuff
-- should be moved out, now that this project is being forked off.


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
testMenu = spinner ["foo", "bar", "baz"]

||| A component that represents a user-chosen value
data TestModal = Default String | Selected String String

||| Implement show for the component (and thereby View)
Show TestModal where
  show (Default label)    = "\{label}\nNo Selection"
  show (Selected label c) = "\{label}\n\{c}"

||| Construct a TestModal component
testModal2 : Component String
testModal2 = component @{show} (Default header) onKey unavailable
  where
    header : String
    header = "Modal 2: (a): Baz, (b): Quux, (c): From Spinner"

    onSelect : Maybe String -> TestModal
    onSelect Nothing  = Default  header
    onSelect (Just s) = Selected header s

    onKey : Component.Handler TestModal String Key
    onKey (Alpha 'a') _              = yield "Baz"
    onKey (Alpha 'b') _              = yield "Quux"
    onKey (Alpha 'c') s              = push testMenu onSelect
    onKey Enter       s@(Default _)  = ignore
    onKey Enter       (Selected _ s) = yield s
    onKey Escape      _              = exit
    onKey _           _              = ignore

testModal1 : Component String
testModal1 = component @{show} (Default header) onKey unavailable
  where
    header : String
    header = "Modal 1: (a): Foo, (b): Bar, (c): From Modal 2"

    onSelect : Maybe String -> TestModal
    onSelect Nothing = Default header
    onSelect (Just s) = Selected header s

    onKey : Component.Handler TestModal String Key
    onKey (Alpha 'a') _              = yield "Foo"
    onKey (Alpha 'b') _              = yield "Bar"
    onKey (Alpha 'c') s              = push testModal2 onSelect
    onKey Enter       s@(Default _)  = ignore
    onKey Enter       (Selected _ s) = yield s
    onKey Escape      _              = exit
    onKey _           _              = ignore

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
  let result : Maybe (HVect [String, Nat, Integer, Double, String]) = !(runComponent testForm)
  case result of
    Nothing => putStrLn "User Canceled"
    Just choice => putStrLn $ "User selected: \{show choice}"
