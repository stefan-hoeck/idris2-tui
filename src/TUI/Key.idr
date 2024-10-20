||| This module implements keyboard event handling.
|||
||| For now, only a small portion of legacy ANSI key sequences are
||| supported. The goal is to expand this to support progressive
||| enhancement towards the modern iTerm2-style escape sequences.
module TUI.Key


import Derive.Prelude
import JSON.Derive
import TUI.Event
import TUI.DFA


%default total
%language ElabReflection


||| Abstract key value, decoded from ANSI escape sequence.
|||
||| Right now we support only the bare minimum required for basic
||| UI. In particular, we have no support for modifier or function
||| keys.
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
|||
||| XXX: After heavy refactoring, this type has become isomorphic to
||| Maybe (SnocList Char).
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

||| A machine which decodes (some) legacy-style ANSI sequences.
export
ansiDecoder : Automaton Char Key
ansiDecoder = automaton Default decode
  where
    ||| Decide what to do for the given character.
    |||
    ||| This is just a quick-and-dirty MVP. No attempt to decode
    ||| modifiers.
    |||
    ||| The eventual goal is to use the code in `TUI/DFA.idr` to make
    ||| implementing the full spec more manageable. For now, this
    ||| simple approach based on pattern gets the job done.
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

||| An event source which decodes raw `Char`s into `Key` values.
export
onAnsiKey : IO (Event Key)
onAnsiKey = decoded "Stdin" ansiDecoder
