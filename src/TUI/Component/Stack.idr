||| Minimalist terminal UI framework.
|||
module TUI.Component.Stack

import TUI.Component


%default total


||| A heterogenous stack of Components.
|||
||| @top  The top type of the stack
||| @root The root type of the stack.
public export
data Stack : (top : Type) -> (root : Type) -> Type where
  Nil  : Stack root root
  ||| @merge Function to merge top of the stack with the element beneath.
  (::) : (merge : Maybe top -> Component a) -> Stack a root -> Stack top root

||| A Component which Introduces a Modal Context.
|||
||| @rootT Type of the value yielded by context.
|||
||| A component manages a stack of components, routing events to the
||| topmost component.
|||
||| When the top component responds with `Yield`, it is removed from
||| the stack, and the yielded value merged into the next stack
||| element, if any exists. If the stack is empty, then the `Modal`
||| itself yields the root value.
public export
record Modal rootT where
  constructor M
  component : Component topT
  stack : Stack topT rootT

||| Remove the top component from the modal stack.
|||
||| @self The top-most modal component
||| @v    The value to merge or yield.
pop
  :  (self : Modal rootT)
  -> (v : Maybe self.topT)
  -> Either (Modal rootT) (Maybe rootT)
pop (M top [])               v = Right v
pop (M top (merge :: tail))  v = Left $ M (merge v) tail

||| Push a new component onto the Modal context.
export
push
  :  Component top
  -> (cur : Modal rootT)
  -> (Maybe top -> Component cur.topT)
  -> Modal rootT
push t cur f = M t (f :: cur.stack)

||| delegate event handling to the wrapped component
export
handle : Handler (Modal rootT) rootT
handle key self = case handle key self.component of
  Ignore => Ignore
  Yield x => case pop self x of
    Left next => Do next
    Right v => Yield v
  Do x  => Do $ {component := x} self
  Run x => Run $ do pure $ {component := !x} self
  Push t m => Do $ push t self m

export
View (Modal t) where
  size self = size self.component
  paint state window self = paint state window self.component

||| Lift a modal to a component
export
modal
  : Modal t
  -> Component t
modal m = MkComponent {
  State = Modal t,
  state = m,
  handler = handle,
  vimpl = %search
}

||| Construct a modal root component.
|||
||| Only root components can handle the `Push` response.
export
root
  : View stateT
  => stateT
  -> Handler stateT valueT
  -> Component valueT
root init handler = modal $ M (active init handler) []
