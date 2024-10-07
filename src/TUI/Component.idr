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

mutual
  ||| Application-supplied response to an input event.
  |||
  ||| @ Ignore Don't do anything in response to this event
  ||| @ Yield  Yield a value to the parent controller or runtime.
  ||| @ Do     Update the model via an action.
  ||| @ Run    Perform an IO action on the model.
  public export
  data Response stateT valueT
    = Ignore
    | Yield (Maybe  valueT)
    | Do    (Update stateT)
    | Run   (Effect stateT)
    | Push  (Component topT) (Maybe topT -> stateT)

  ||| A function to update state in response to a key press.
  public export
  0 Handler : Type -> Type -> Type
  Handler stateT valueT = Key -> stateT -> Response stateT valueT

  ||| A component pairs a viewable state with a handler.
  |||
  ||| Because of mutual recursion with Response, component cannot be
  ||| defined via `record`, so it is defined here via `data`, with the
  ||| projection functions implemented manually.
  public export
  data Component : Type -> Type where
    MkComponent
      :  (0 State : Type)
      -> (state   : State)
      -> (handler : Handler State valueT)
      -> (vimpl   : View State)
      -> Component valueT

export
0 (.State) : Component valueT -> Type
(.State) (MkComponent s _ _ _) = s

export
(.state) : (self : Component valueT) -> self.State
(.state) (MkComponent _ s _ _) = s

export
(.handler) : (self : Component valueT) -> Handler self.State valueT
(.handler) (MkComponent _ _ h _) = h

export
(.vimpl) : (self : Component valueT) -> View self.State
(.vimpl) (MkComponent _ _ _ v) = v

||| The result of updating a component
public export
0 Result : Type -> Type -> Type
Result stateT valueT = Either stateT (Maybe valueT)

||| A no-op handler
ignore : Handler stateT valueT
ignore _ _ = Ignore

||| A component wraps a View, so a component is also a view.
export
View (Component _) where
  size self = size @{self.vimpl} self.state
  paint state window self = paint @{self.vimpl} state window self.state

||| This is the top-level handler for component, which is used by
||| `runComponent` in MainLoop.idr
export
handle : Handler (Component valueT) valueT
handle key self = case self.handler key self.state of
  Ignore   => Ignore
  Yield v  => Yield v
  Do  s    => Do $ {state := s} self
  Run e    => Run $ do pure $ {state := !e} self
  Push t m => Push t $ \v => ({state := m v} self)

||| Wrap a plain view to construct a static interface element.
export
static
  : View stateT
  => stateT
  -> Component valueT
static init = MkComponent {
  State = stateT,
  state = init,
  handler = ignore,
  vimpl = %search
}

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

||| Lift a Response to an (IO Result).
|||
||| Essentially, this interprets the `Response` DSL keywords. It is
||| called by runComponent in MainLoop.idr.
export
liftUpdate
  :  Response stateT valueT
  -> stateT
  -> IO (Result stateT valueT)
liftUpdate Ignore     self = pure $ Left self
liftUpdate (Yield x)  _    = pure $ Right x
liftUpdate (Do next)  _    = pure $ Left next
liftUpdate (Run next) _    = pure $ Left !next
liftUpdate (Push t m) self = assert_total $ idris_crash "unhandled Push"
