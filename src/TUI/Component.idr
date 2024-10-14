||| Minimalist terminal UI framework.
|||
||| A component combines a view and a controller.
module TUI.Component


import public TUI.View
import public TUI.Event

%default total

-- forward-declare these types, because mutual blocks don't work with
-- records.
public export data Response : Type -> Type -> Type
public export 0    Handler  : Type -> Type -> Type

||| A component pairs a viewable state with a handler.
public export covering
record Component valueT where
  constructor MkComponent
  0 State : Type
  state   : State
  handler : Component.Handler State valueT
  vimpl   : View State

||| The result of handling an event
public export
data Response stateT valueT
  = Continue (IO stateT)
  | Yield valueT
  | Exit
  | Push (Component a) (Maybe a -> stateT)

export
continue : IO stateT -> IO (Response stateT _)
continue state = pure $ Continue $ state

export
update : stateT -> IO (Response stateT _)
update state = pure $ Continue $ pure state

export
yield : valueT -> IO (Response _ valueT)
yield value = pure $ Yield value

export
exit : IO (Response _ _)
exit = pure $ Exit

export
exitWith : Maybe valueT -> IO (Response _ valueT)
exitWith Nothing  = exit
exitWith (Just v) = yield v

export
ignore : {auto self : stateT} -> IO (Response stateT _)
ignore = update self

export
push
  :  (Component topT)
  -> (Maybe topT -> stateT)
  -> IO (Response stateT valueT)
push c m = pure $ Push c m

Handler stateT valueT = Key -> stateT -> IO $ Response stateT valueT

||| A component wraps a View, so a component is also a view.
export
View (Component _) where
  size self = size @{self.vimpl} self.state
  paint state window self = paint @{self.vimpl} state window self.state

||| Construct an active component by supplying both view and handler.
export
active
  : View stateT
  => stateT
  -> Component.Handler stateT valueT
  -> Component valueT
active init handler = MkComponent {
  State = stateT,
  state = init,
  handler = handler,
  vimpl = %search
}
