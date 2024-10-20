module TUI.Key


import Derive.Prelude
import JSON.Derive
import TUI.Event
import TUI.DFA


%default total
%language ElabReflection


||| Abstract key value, decoded from ANSI escape sequence.
public export
data Key
  = Alpha Char
  | Left
  | Right
  | Up
  | Down
  | Delete
  | Enter
  | Tab
  | Escape
%runElab derive "Key" [Ord, Eq, Show, FromJSON]

||| The state machine for escape sequence decoding.
data EscState
  = HaveEsc (SnocList Char)
  | Default
%runElab derive "EscState" [Show]

||| Capture char into escape buffer.
escape : Char -> EscState -> Transition EscState Key
escape c (HaveEsc e) = Advance (HaveEsc (e :< c)) Nothing
escape c Default     = Advance (HaveEsc [< c])    Nothing

||| Emit the given key and reset state.
emit : Key -> Transition EscState Key
emit key = Advance Default (Just key)

reset : Transition EscState Key
reset = Advance Default Nothing

||| Decide what to do for the given character.
|||
||| This is just a quick-and-dirty MVP. No attempt to decode
||| modifiers.
decode : TransitionFn EscState Char Key
-- multi-char sequences in this clause
decode c self@(HaveEsc esc) = case esc :< c of
  [<]                  => escape c self
  [< '\ESC']           => escape c self
  [< '\ESC', '\ESC']   => emit Escape
  [< '\ESC', '[']      => escape c self
  [< '\ESC', '[', 'C'] => emit Right
  [< '\ESC', '[', 'D'] => emit Left
  [< '\ESC', '[', 'A'] => emit Up
  [< '\ESC', '[', 'B'] => emit Down
  _                    => reset
-- single-char matches below here
decode '\ESC' self = escape '\ESC' self
decode '\DEL' self = emit Delete
decode '\n'   self = emit Enter
decode '\t'   self = emit Tab
decode c      self = emit (Alpha c)

ansiDecoder : Automaton Char Key
ansiDecoder = automaton Default decode

||| An event source which decodes ANSI escape codes to Keys.
|||
||| The decoded keys are passed to the given key handler.
export
onAnsiKey
  : Handler stateT valueT Key
  -> IO (Event stateT valueT)
onAnsiKey handler = decoded "Stdin" ansiDecoder handler
