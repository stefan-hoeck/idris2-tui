||| Minimalist terminal UI framework.
|||
||| This module provides partial support for decoding ansi input
||| escape sequences into high-level keys. This is the kind of thing
||| that curses would do for us, but we have to do ourselves.
module TUI.Event


import public JSON
import JSON.Derive


%default total
%language ElabReflection


||| A string event tag, and the associated event handler.
public export
record EventSource stateT actionT where
  constructor On
  tag         : String
  {auto impl  : FromJSON eventT}
  handler     : eventT -> stateT -> actionT

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
  -> List (EventSource stateT actionT)
  -> Either String (stateT -> actionT)
decodeNext e sources = case parseJSON Virtual e of
    Left  err => Left "Parse Error: \{show err}"
    Right parsed => loop parsed sources
where
  loop
    : JSON
    -> List (EventSource stateT actionT)
    -> Either String (stateT -> actionT)
  loop parsed []        = Left "Unhandled event: \{e}"
  loop parsed (x :: xs) = case match @{x.impl} x.tag parsed of
      Left  err => loop parsed xs
      Right evt => Right (x.handler evt)

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

||| Replace the inner state type, according to `f`
Functor EscState where
  map f (HaveEsc e s) = HaveEsc e (f s)
  map f (Default s)   = Default (f s)

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
export
accept : Char -> EscState stateT -> EscState stateT
accept c (HaveEsc e s) = HaveEsc (e :< c) s
accept c (Default s)   = HaveEsc [< c]    s

||| Exit escape state
export
reset : EscState stateT -> EscState stateT
reset self = Default $ unwrap self

||| It's handy to be able to `show` the escape state for debugging.
export
Show s => Show (EscState s) where
  show = show . unwrap

||| Response to a receving a character from stdin.
export
data Action actionT
  = Accept Char
  | Reset
  | Emit actionT

||| Map the inner action type according to `f`.
export
Functor Action where
  map _ (Accept c) = (Accept c)
  map _ Reset      = Reset
  map f (Emit a)   = Emit (f a)

||| Decide what to do for the given character.
|||
||| This is just a quick-and-dirty MVP. No attempt to decode
||| modifiers.
export
decode : Char -> EscState stateT -> Action Key
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

||| Interpret console escape sequences as keys.
|||
||| @onKey : Translate a key press into the expected action type.
|||
||| Note: this falls down if the user presses the escape key, because
||| we can't tell the difference between the key press and the start
||| of an escape sequence. The user must press escape twice.
export
handleEsc
  : (Key -> stateT -> actionT)
  -> Char
  -> EscState stateT
  -> Action actionT
handleEsc onKey c self = (flip onKey) (unwrap self) <$> decode c self

||| Update the escape state in response to an action.
|||
||| @update: Update the inner state in response to an inner action.
export
updateEsc
  :  Monad m
  => (actionT -> stateT -> m (Either stateT valueT))
  -> Action actionT
  -> EscState stateT
  -> m (Either (EscState stateT) valueT)
updateEsc _      (Accept c)    self = pure $ Left $ accept c self
updateEsc _      Reset         self = pure $ Left $ reset self
updateEsc update (Emit action) self = case !(update action (unwrap self)) of
  Left  state => pure $ Left $ const state <$> self
  Right value => pure $ Right value

||| Lift an event source to an EscState event source.
export
liftEsc
  :  EventSource stateT actionT
  -> EventSource (EscState stateT) (Action actionT)
liftEsc = { handler $= wrapHandler }
  where
    wrapHandler
      : (eventT -> stateT -> actionT)
      -> eventT
      -> EscState stateT
      -> Action actionT
    wrapHandler original event state = Emit $ original event $ unwrap state
