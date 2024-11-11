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

||| Support for self-contained UI elements.
module TUI.Component


import public TUI.View
import public TUI.Event
import public TUI.Key

%default total

||| The result of handling an event within a component.
|||
||| This type extends `Extends.Result` with the `Push` case, which
||| signals that control should pass to a child component. Because
||| `Push` takes a component as a parameter, it must be defined in
||| this module.
|||
||| Note: this type is forward-declared, because mutual blocks do not
||| work with records, owing to a bug in Idris.
public export data Response : Type -> Type -> Type -> Type

||| A function which handles a key event in a component.
|||
||| This is distinct from `Event.Handler` in that it returns a
||| `Response` rather than a `Result`.
|||
||| Note: this type alias is forward-declared, because mutual blocks
||| do not workw ith records, owing to a bug in Idris.
public export 0 Handler : Type -> Type -> Type -> Type

||| A reusable user-interface element.
|||
||| Component is closely tied to `Stack` and `Modal`.
public export covering
record Component eventT valueT where
  constructor MkComponent
  0 State : Type
  state   : State
  handler : Component.Handler State valueT eventT
  get     : State -> Maybe valueT
  vimpl   : View State

-- implementations for forward-declared types

public export
data Response eventT stateT valueT
  = Continue (IO stateT)
  | Yield valueT
  | Exit
  | Push (Component eventT a) (Maybe a -> stateT)

Handler stateT valueT eventT = eventT -> stateT -> IO $ Response eventT stateT valueT

||| Get the value from the component, if it is available.
export
(.value) : Component eventT valueT -> Maybe valueT
(.value) self = self.get self.state

-- Hide this projection function. we use (.state) instead. This
-- suppresses warnings about `state` being shadowed in the definitions
-- below.
%hide Component.state

||| A component wraps a View, so a component is also a view.
export
View (Component _ _) where
  size self = size @{self.vimpl} self.state
  paint state window self = paint @{self.vimpl} state window self.state


||| These definitions make writing event handlers a bit nicer.
namespace ComponentDSL

  ||| A generic response: update with the given IO action.
  export
  continue : IO stateT -> IO (Response _ stateT _)
  continue state = pure $ Continue $ state

  ||| A generic response: update with the given value.
  export
  update : stateT -> IO (Response _ stateT _)
  update state = pure $ Continue $ pure state

  ||| A generic response: yield given value to the parent, or exit.
  export
  yield : valueT -> IO (Response _ _ valueT)
  yield value = pure $ Yield value

  ||| A generic response: exit.
  export
  exit : IO (Response _ _ _)
  exit = pure $ Exit

  ||| A generic response: yield or exit, depending on argument.
  export
  exitWith : Maybe valueT -> IO (Response _ _ valueT)
  exitWith Nothing  = exit
  exitWith (Just v) = yield v

  ||| A generic response: do nothing.
  export
  ignore : {auto self : stateT} -> IO (Response _ stateT _)
  ignore = update self

  ||| A generic response: yield if value is `Just`, ignore if nothing
  export
  exitIf : {auto self : stateT} -> Maybe valueT -> IO (Response _ stateT valueT)
  exitIf Nothing  = ignore
  exitIf (Just v) = yield v

  ||| A generic response: pass control to the given child component.
  |||
  ||| When the component yields or exits, the result is folded into the
  ||| current component via `merge`.
  export
  push
    :  {0 eventT : Type}
    -> (top   : Component eventT topT)
    -> (merge : Maybe topT -> stateT)
    -> IO (Response eventT stateT valueT)
  push top merge = pure $ Push top merge

  ||| A getter function which always returns Nothing
  |||
  ||| This is for components which cannot yield a partial value, or
  ||| which are still in development.
  export
  unavailable : stateT -> Maybe valueT
  unavailable _ = Nothing

||| Update a component in response to an event.
|||
||| This wraps the stored handler, which operates on `State`, turning
||| it into the component-level handler which is needed by client
||| code.
export
handle : Component.Handler (Component eventT valueT) valueT eventT
handle event self = case !(self.handler event self.state) of
  Continue state => update $ {state := !state} self
  Yield result   => yield result
  Exit           => exit
  Push top merge => push top $ updateInner merge
where
  updateInner : (Maybe a -> self.State) -> Maybe a -> Component eventT valueT
  updateInner merge result = {state := merge result} self

||| Construct a component by supplying both view and key handler.
|||
||| This will become the new definition of component once we finish migrating.
export
component
  : View stateT
  => (state    : stateT)
  -> (handler  : Component.Handler stateT valueT eventT)
  -> (get      : stateT -> Maybe valueT)
  -> Component eventT valueT
component init handler get = MkComponent {
  State = stateT,
  state = init,
  handler = handler,
  get = get,
  vimpl = %search
}

namespace User
  ||| Type alias for handlers that accept user-defined events.
  public export
  0 Handler
    :  {0 events : List Type}
    -> Type
    -> Type
    -> Type
    -> Type
  Handler {events} stateT valueT eventT =
    eventT -> stateT -> IO $ Response (HSum events) stateT valueT

  ||| Turn a list of handlers into a single handler over a set of
  ||| events.
  export
  union
    :  {0 events : List Type}
    -> {0 stateT, valueT : Type}
    -> All (User.Handler {events} stateT valueT) events
    -> Component.Handler stateT valueT (HSum events)
  union handlers event state = go handlers event state
    where
      go
        :  All (Handler {events} stateT valueT) a
        -> HSum a
        -> stateT
        -> IO $ Response (HSum events) stateT valueT
      go (h :: _ ) (Here  e) s = h e s
      go (_ :: hs) (There e) s = go hs e s

||| Take a `Component _ a` to a `Component _ b` via `f`.
|||
||| This is useful for specializing library components to
||| application-specific types.
|||
||| The function `f` applies only to the yield value, leaving the
||| internal state and interactive behavior unchanged.
|||
||| Example: Take a generic numeric component and produce a
||| length. Here, `(.mm`) is a function `Double -> Length`.
|||
|||    inputMM : Length -> Component Length
|||    inputMM length = (.mm) <$> numeric (length.convertTo MilliMeters)
export
Functor (Component eventT) where
  map f wrapped = component @{wrapped.vimpl} wrapped.state handle get
    where
      get : wrapped.State -> Maybe b
      get self = f <$> wrapped.get self

      handle : Component.Handler wrapped.State b eventT
      handle event state = case !(wrapped.handler event state) of
        Continue state => pure $ Continue state
        Yield result   => yield $ f result
        Exit           => exit
        Push top merge => push top merge

||| This is like above `map`, but takes a partial function.
|||
||| This is useful when you want to do further processing or
||| validation on the yield value.
|||
||| Note: if the wrapped component yields, but `f` returns Nothing,
||| the response is equivalent to `ignore`, rather than `exit`. The
||| assumption is that the component has failed validation, and so the
||| component should remain in place.
|||
||| XXX: does this correspond to a prelude interface?
export
mapMaybe : (a -> Maybe b) -> Component e a -> Component e b
mapMaybe f wrapped = component @{wrapped.vimpl} wrapped.state handle get
  where
    get : wrapped.State -> Maybe b
    get self = join $ f <$> wrapped.get self

    handle : Component.Handler wrapped.State b e
    handle event state = case !(wrapped.handler event state) of
      Continue state => pure $ Continue state
      Yield result   => exitIf $ f result
      Exit           => exit
      Push top merge => push top merge

||| Cute alias for `mapMaybe' above.
export
(<$?>) : (a -> Maybe b) -> Component e a -> Component e b
(<$?>) f c = mapMaybe f c
