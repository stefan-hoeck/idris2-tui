||| Minimalist terminal UI framework.
|||
module TUI.Component.Modal

import TUI.Component


%default total


||| A heterogenous stack of Components.
|||
||| @top  The top type of the stack
||| @root The root type of the stack.
|||
||| Note: This was a tricky data-strcuture to get right. One key
||| detail is that the top of the stack is a function which returns a
||| component. The other key detail is the `top` index, which allows
||| the type checker to recognize the empty stack at the type level.
|||
||| I've tried other variatons on this idea, and this seems to be the
||| best.
public export
data Stack : (top : Type) -> (root : Type) -> Type where
  Nil  : Stack root root
  ||| @merge Function to merge top of the stack with the element beneath.
  (::) : (merge : Maybe top -> Component a) -> Stack a root -> Stack top root

||| A context for modal interaction.
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
|||
||| Note: this type is needed to hide the `top` index on the stack,
||| otherwise we could just use `Stack` itself as a component.
public export
record Modal rootT where
  constructor M
  component : Component topT
  stack : Stack topT rootT

-- we can use `(.topT)` instead, which reads better anyway. `topT`
-- just ends up shadowing useful type arguments. I wish for some
-- better control over record elaboration in future versions of Idris.
--
-- the other way to handle this is to disable unbound implicits, but
-- this then requires explicit quantification, which is overkill here.
%hide Modal.topT

||| Remove the top component from the modal stack.
|||
||| @self   The current modal context.
||| @result The value to merge or yield.
pop
  :  (self : Modal rootT)
  -> (result : Maybe self.topT)
  -> Either (Modal rootT) (Maybe rootT)
pop (M top [])               result = Right result
pop (M top (merge :: tail))  result = Left $ M (merge result) tail

||| Push a component into the Modal context, with the merge function.
push
  :  Component topT
  -> (self : Modal rootT)
  -> (Maybe topT -> Component self.topT)
  -> Modal rootT
push top self merge = M top (merge :: self.stack)

||| Construct a new modal context with the given component.
|||
||| You shouldn't need to call this. It is used by `runComponent` in
||| the `MainLoop` to wrap the root component.
export
root : Component rootT -> Modal rootT
root component = M component []

||| This is the top-level event handler for a modal context.
|||
||| You should not normally need to call this: it is called by
||| `runComponent` in MainLoop.
export
handle : Event.Handler (Modal rootT) rootT eventT
{-
handle event self = case !(handle event self.component) of
  Continue state => update $ {component := !state} self
  Yield result   => doPop (Just result)
  Exit           => doPop Nothing
  Push top merge => update $ push top self merge
where
  doPop : (Maybe self.topT) -> Result (Modal rootT) rootT
  doPop result = case pop self result of
    Left  state  => update state
    Right result => exitWith result
-}

||| This is the default view implementation for Modal.
|||
||| It paints only the top-most component into the given window.
export
View (Modal t) where
  size self = size self.component
  paint state window self = paint state window self.component
