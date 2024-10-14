||| Minimalist terminal UI framework.
|||
||| The definitions in this module comprise an embedded DSL for
||| building re-usable interactive components.
module TUI.Component


import public TUI.View
import public TUI.Event

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
public export data Response : Type -> Type -> Type

||| A function which handles a key event in a component.
public export 0    Handler  : Type -> Type -> Type

||| A reusable user-interface element.
|||
||| Component works together tied to `Stack` and `Modal`.
|||
|||
public export covering
record Component valueT where
  constructor MkComponent
  0 State : Type
  state   : State
  handler : Component.Handler State valueT
  vimpl   : View State

public export
data Response stateT valueT
  = Continue (IO stateT)
  | Yield valueT
  | Exit
  | Push (Component a) (Maybe a -> stateT)

Handler stateT valueT = Key -> stateT -> IO $ Response stateT valueT

%hide Component.state

||| A component wraps a View, so a component is also a view.
export
View (Component _) where
  size self = size @{self.vimpl} self.state
  paint state window self = paint @{self.vimpl} state window self.state


||| These functions define a DSL for handling events.
namespace EventDSL

  ||| A generic response: update with the given IO action.
  export
  continue : IO stateT -> IO (Response stateT _)
  continue state = pure $ Continue $ state

  ||| A generic response: update with the given value.
  export
  update : stateT -> IO (Response stateT _)
  update state = pure $ Continue $ pure state

  ||| A generic response: yield given value to the parent, or exit.
  export
  yield : valueT -> IO (Response _ valueT)
  yield value = pure $ Yield value

  ||| A generic response: exit.
  export
  exit : IO (Response _ _)
  exit = pure $ Exit

  ||| A generic response: yield or exit, depending on argument.
  export
  exitWith : Maybe valueT -> IO (Response _ valueT)
  exitWith Nothing  = exit
  exitWith (Just v) = yield v

  ||| A generic response: do nothing.
  export
  ignore : {auto self : stateT} -> IO (Response stateT _)
  ignore = update self

  ||| A generic response: pass control to the given child component.
  |||
  ||| When the component yields or exits, the result is folded into the
  ||| current component via `merge`.
  export
  push
    :  (top   : Component topT)
    -> (merge : Maybe topT -> stateT)
    -> IO (Response stateT valueT)
  push c m = pure $ Push c m

||| Update a component in response to an event.
|||
||| This wraps the stored handler, which operates on `State`, turning
||| it into the component-level handler which is needed by client
||| code.
export
handle : Component.Handler (Component valueT) valueT
handle key self = case !(self.handler key self.state) of
  Continue state => update $ {state := !state} self
  Yield result   => yield result
  Exit           => exit
  Push top merge => push top $ updateInner merge
where
  updateInner : (Maybe a -> self.State) -> Maybe a -> Component valueT
  updateInner merge result = {state := merge result} self

||| Construct a component by supplying both view and handler.
export
component
  : View stateT
  => stateT
  -> Component.Handler stateT valueT
  -> Component valueT
component init handler = MkComponent {
  State = stateT,
  state = init,
  handler = handler,
  vimpl = %search
}
