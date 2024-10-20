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
import Data.IORef
import JSON.Derive
import TUI.DFA


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

||| Decodes the given event type from Stdin.
|||
||| XXX: The original intent of this type was to collect input from a
||| dedicated thread, injecting it into the main event queue. But
||| right now, all events are decoded as JSON records received on
||| stdin.
|||
||| The tag identifies the event type, whose contents are then decoded
||| from JSON. The resulting Idris value is then passed to a decoder
||| state machine, and then finally, to the asociated event handler.
export
record Event stateT valueT where
  constructor On
  0 RawEventT : Type
  0 EventT    : Type
  tag         : String
  {auto impl  : FromJSON RawEventT}
  decoder     : IORef $ Automaton RawEventT EventT
  handler     : Handler stateT valueT EventT

||| Construct an EventSource for an event that must be further decoded.
export
decoded
  :  {0 rawEventT, eventT : Type}
  -> FromJSON rawEventT
  => String
  -> (decoder : Automaton rawEventT eventT)
  -> Handler stateT valueT eventT
  -> IO (Event stateT valueT)
decoded tag decoder handler = pure $ On {
  RawEventT = rawEventT,
  EventT = eventT,
  tag = tag,
  decoder = !(newIORef decoder),
  handler = handler
}

||| Construct an event source for an event that can be used directly.
export
raw
  :  {0 eventT : Type}
  -> FromJSON eventT
  => String
  -> Handler stateT valueT eventT
  -> IO (Event stateT valueT)
raw tag handler = decoded tag identity handler

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
||| The internal decoder state is updated, if need be, and the
||| resulting handler is returned with the event partially applied, so
||| that it need not be mentioned in the return type.
export
decodeNext
  :  String
  -> List (Event stateT valueT)
  -> IO (Either String (stateT -> Result stateT valueT))
decodeNext e sources = case parseJSON Virtual e of
    Left  err => pure $ Left "Parse Error: \{show err}"
    Right parsed => loop parsed sources
where
  loop
    : JSON
    -> List (Event stateT valueT)
    -> IO (Either String (stateT -> Result stateT valueT))
  loop parsed []        = pure $ Left "Unhandled event: \{e}"
  loop parsed (x :: xs) = case match @{x.impl} x.tag parsed of
      Left  err => loop parsed xs
      Right evt => case next evt !(readIORef x.decoder) of
        Discard      => pure $ Right $ \_ => ignore
        Advance y z  => do
          writeIORef x.decoder y
          case z of
            Nothing => pure $ Right $ \_ => ignore
            Just e  => pure $ Right $ x.handler e
        Accept y => pure $ Left $ "Toplevel event decoder in final state!"
        Reject err => pure $ Left "Decode error: \{err}"
