||| Minimalist terminal UI framework.
|||
module TUI.Component.Stack

import TUI.Component
--import TUI.Geometry
--import TUI.View


%default total

||| A heterogenous stack of Components.
|||
||| @top  The top type of the stack
||| @root The root type of the stack.
data Stack : (top : Type) -> (root : Type) -> Type where
  Nil  : Stack root root
  ||| @f Merges the top of the stack with the next lower element.
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
export
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

||| Construct a new `Modal` context with the given root component.
export
root : Component rootT -> Modal rootT
root init = M init []

||| Push a new component onto the Modal context.
export
push
  :  Component top
  -> (cur : Modal rootT)
  -> (top -> Component cur.topT -> Component cur.topT)
  -> Modal rootT
push t cur f = M t (update :: cur.stack)
  where
    update : Maybe top -> Component cur.topT
    update Nothing = cur.component
    update (Just v) = f v cur.component

||| Controller implementation for Modal
export
Controller (Modal rootT) rootT where
  handle key self = case handle key self.component of
    Ignore => Ignore
    Yield x => case pop self x of
      Left next => Do next
      Right v => Yield v
    Do x  => Do $ {component := x} self
    Run x => Run $ do pure $ {component := !x} self
