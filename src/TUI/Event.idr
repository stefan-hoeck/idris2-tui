||| Minimalist terminal UI framework.
|||
||| This module provides partial support for decoding ansi input
||| escape sequences into high-level keys.
|||
||| There are libraries that do this, but not ones written in
||| Idris. This seems like territory for which Idris is well-suited,
||| and I don't feel like getting into Idris FFI right now.
|||
||| At present, only a few core sequences are decoded that should work
||| anywhere. In the future, I will expand this to cover iTerm2-style
||| extensions supported by contemporary terminals, but this will
||| require support for feature detection.
module TUI.Event


import public JSON
import JSON.Derive


%default total
%language ElabReflection


namespace Result
  ||| A function to update state in response to an event.
  public export
  0 Result : Type -> Type -> Type
  Result stateT valueT = IO (Either stateT (Maybe valueT))

  export
  ignore : {auto self : stateT} -> Result stateT _
  ignore {self} = pure $ Left self

  export
  exitWith : Maybe valueT -> Result _ valueT
  exitWith result = pure $ Right result

  export
  exit : Result _ _
  exit = exitWith Nothing

  export
  yield : valueT -> Result _ valueT
  yield v = exitWith $ Just v

  export
  update : stateT -> Result stateT _
  update next = pure $ Left next

  export
  run : IO stateT -> Result stateT _
  run next = pure $ Left !next

||| A function to update application state in response to an event.
public export
0 Handler : Type -> Type -> Type -> Type
Handler stateT valueT eventT = eventT -> stateT -> Result stateT valueT

||| A string event tag, and the associated event handler.
|||
||| XXX: The original intent of this type was to collect input from a
||| dedicated thread, injecting it into the main event queue. But
||| right now, all events are decoded as JSON records received on
||| stdin.
|||
||| The tag identifies the event type, whose contents are then
||| decoded. The resulting event is passed to the handler, to yield
||| the output result.
public export
record Event stateT valueT where
  constructor On
  tag         : String
  0 Event     : Type
  {auto impl  : FromJSON Event}
  handler     : Handler stateT valueT Event

||| Decode the top-level record in the given JSON.
|||
||| This is somewhat manual, since there's no outer Sum type to
||| decode into. We directly decode the first element of `contents`,
||| which is where our event type actually lives.
|||
||| XXX: Probably the JSON package has this functionality, but I
||| didn't want to go down that rabbit hole, so I wrote it manually.
match : FromJSON a => String -> JSON -> Either String a
match expected (JObject [
  ("tag", JString got),
  ("contents", (JArray [rest]))
]) =
  if expected == got
  then case fromJSON rest of
    Left err => Left $ show err
    Right v  => Right v
  else Left "Incorrect tag"
match _ _ = Left "Wrong shape"

||| Try each handler in succession until one decodes an event.
|||
||| Returns the original handler, with the event partially applied
||| (which avoids having to mention it in the return type).
export
decodeNext
  : String
  -> List (Event stateT valueT)
  -> Either String (stateT -> Result stateT valueT)
decodeNext e sources = case parseJSON Virtual e of
    Left  err => Left "Parse Error: \{show err}"
    Right parsed => loop parsed sources
where
  loop
    : JSON
    -> List (Event stateT valueT)
    -> Either String (stateT -> Result stateT valueT)
  loop parsed []        = Left "Unhandled event: \{e}"
  loop parsed (x :: xs) = case match @{x.impl} x.tag parsed of
      Left  err => loop parsed xs
      Right evt => Right $ x.handler evt

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
