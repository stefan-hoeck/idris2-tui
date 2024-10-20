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


import Data.List.Quantifiers.Extra
import Data.IORef
import public JSON
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
||| state machine.
export
record Event eventT where
  constructor On
  0 RawEventT : Type
  tag         : String
  {auto impl  : FromJSON RawEventT}
  decoder     : IORef $ Automaton RawEventT eventT

||| Construct an EventSource for an event that must be further decoded.
export
decoded
  :  {0 rawEventT, eventT : Type}
  -> FromJSON rawEventT
  => String
  -> (decoder : Automaton rawEventT eventT)
  -> IO (Event eventT)
decoded tag decoder = pure $ On {
  RawEventT = rawEventT,
  tag       = tag,
  decoder   = !(newIORef decoder)
}

||| Construct an event source for an event that can be used directly.
export
raw
  :  {0 eventT : Type}
  -> FromJSON eventT
  => String
  -> IO (Event eventT)
raw tag = decoded tag identity

||| Decode the top-level record in the given JSON.
|||
||| XXX: Probably the JSON package can work with HSum more directly,
||| but I didn't want to go down that rabbit hole, so I wrote it
||| manually.
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
||| Returns the event as an HSum over all the event sources.
export
decodeNext
  :  String
  -> All Event tys
  -> IO $ Either String (HSum tys)
decodeNext e sources = case parseJSON Virtual e of
    Left  err    => pure $ Left "Parse Error: \{show err}"
    Right parsed => loop parsed sources
where
  loop
    : JSON
    -> All Event a
    -> IO (Either String (HSum a))
  loop parsed []        = pure $ Left "Unhandled event: \{e}"
  loop parsed (x :: xs) = case match @{x.impl} x.tag parsed of
      Left  err => pure $ There <$> !(loop parsed xs)
      Right evt => case next evt !(readIORef x.decoder) of
        Discard  => pure $ Left "Event discarded."
        Advance state output => do
          writeIORef x.decoder state
          case output of
            Nothing     => pure $ Left "No event decoded."
            Just event  => pure $ Right $ inject event
        Accept y => pure $ Left $ "Toplevel event decoder in final state!"
        Reject err => pure $ Left "Decode error: \{err}"

||| Try to handle an event using one of the given handlers.
|||
||| The event must be covered by one of the handlers in the list.
export
handleEvent
  :  {0 stateT, valueT : Type}
  -> HSum tys
  -> stateT
  -> All (Handler stateT valueT) tys
  -> Result stateT valueT
handleEvent (Here  x)  state (h :: hs) = h x state
handleEvent (There xs) state (h :: hs) = handleEvent xs state hs
