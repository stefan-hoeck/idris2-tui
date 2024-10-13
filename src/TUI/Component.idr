||| Minimalist terminal UI framework.
|||
||| A component combines a view and a controller.
module TUI.Component


import public TUI.View
import public TUI.Event

%default total


||| A function which yields the next state.
public export
0 Update : Type -> Type
Update stateT = stateT

||| A function which yields an IO action of the next state.
public export
0 Effect : Type -> Type
Effect stateT = IO stateT

||| A component pairs a viewable state with a handler.
public export
record Component valueT where
  constructor MkComponent
  0 State : Type
  state   : State
  handler : Handler State valueT
  vimpl   : View State

||| A component wraps a View, so a component is also a view.
export
View (Component _) where
  size self = size @{self.vimpl} self.state
  paint state window self = paint @{self.vimpl} state window self.state

||| This is the top-level handler for component, which is used by
||| `runComponent` in MainLoop.idr
export
handle : Handler (Component valueT) valueT
handle key self = case !(self.handler key self.state) of
  Left  state  => pure $ Left $ {state := state} self
  Right result => pure $ Right result

||| Wrap a plain view to construct a static interface element.
export
static
  : View stateT
  => stateT
  -> Component valueT
static init = MkComponent {
  State = stateT,
  state = init,
  handler = const (\self => Result.ignore {self = self}), -- see note
  vimpl = %search
}

-- Note: XXX, make it easier to write this exact thing.

||| Construct an active component by supplying both view and handler.
export
active
  : View stateT
  => stateT
  -> Handler stateT valueT
  -> Component valueT
active init handler = MkComponent {
  State = stateT,
  state = init,
  handler = handler,
  vimpl = %search
}
