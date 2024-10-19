module TUI.Key


import Derive.Prelude
import JSON.Derive
import TUI.Event


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
export
data EscState stateT
  = HaveEsc (SnocList Char) stateT
  | Default stateT

update : a -> EscState a -> EscState a
update next (HaveEsc e _) = HaveEsc e next
update next (Default _)   = Default next

||| Wrap the inner state type in an EscState
export
wrap : stateT -> EscState stateT
wrap = Default

||| Project the wrapped value from the escape sequence state.
export
unwrap : EscState stateT -> stateT
unwrap (HaveEsc _ s) = s
unwrap (Default   s) = s

||| Enter escape state with empty buffer
accept : Char -> EscState stateT -> EscState stateT
accept c (HaveEsc e s) = HaveEsc (e :< c) s
accept c (Default s)   = HaveEsc [< c]    s

||| Exit escape state
reset : EscState stateT -> EscState stateT
reset self = Default $ unwrap self

||| It's handy to be able to `show` the escape state for debugging.
export
Show s => Show (EscState s) where
  show = show . unwrap

||| Response to a receving a character from stdin.
export
data Action
  = Accept Char -- accept escaped character.
  | Reset       -- discard any existing escaped characters.
  | Emit Key    -- emit the action resulting from the key press.

||| Decide what to do for the given character.
|||
||| This is just a quick-and-dirty MVP. No attempt to decode
||| modifiers.
export
decode : Char -> EscState stateT -> Action
-- multi-char sequences in this clause
decode c (HaveEsc esc _) = case esc :< c of
  [<]                  => Accept c
  [< '\ESC']           => Accept c
  [< '\ESC', '\ESC']   => Emit Escape
  [< '\ESC', '[']      => Accept c
  [< '\ESC', '[', 'C'] => Emit Right
  [< '\ESC', '[', 'D'] => Emit Left
  [< '\ESC', '[', 'A'] => Emit Up
  [< '\ESC', '[', 'B'] => Emit Down
  _                    => Reset
-- single-char matches below here
decode '\ESC' (Default _) = Accept '\ESC'
decode '\DEL' (Default _) = Emit Delete
decode '\n'   (Default _) = Emit Enter
decode '\t'   (Default _) = Emit Tab
decode c      (Default _) = Emit (Alpha c)

||| Convert a Key event handler into a Char event handler by decoding
||| escape sequences.
|||
||| @ onKey : Converts a Key to an action.
decodeANSI
  : (Key -> stateT -> Result stateT valueT)
  -> Char
  -> EscState stateT
  -> Result (EscState stateT) valueT
decodeANSI onKey c self = inner (decode c self) where
  inner : Action  -> Result (EscState stateT) valueT
  inner (Accept c) = pure $ Left $ accept c self
  inner Reset      = pure $ Left $ reset self
  inner (Emit key) = case !(onKey key (unwrap self)) of
    Left  state => pure $ Left $ Default state
    Right value => pure $ Right value

||| A Char EventSource that calls the given handler when a key is
||| decoded.
export
onAnsiKey
  : (Key -> stateT -> Result stateT valueT)
  -> Event (EscState stateT) valueT
onAnsiKey handler = On "Stdin" Char $ decodeANSI handler

||| Handles wrapping / unwrapping EscState from non-keyboard handlers.
export
liftEsc
  :  Event stateT valueT
  -> Event (EscState stateT) valueT
liftEsc = { handler $= wrapHandler }
  where
    wrapHandler
      : (e -> stateT -> Result stateT valueT)
      -> e
      -> EscState stateT
      -> Result (EscState stateT) valueT
    wrapHandler original event self = case !(original event (unwrap self)) of
      Left  x => pure $ Left  $ update x self
      Right x => pure $ Right x
