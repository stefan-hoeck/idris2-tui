||| Minimalist terminal UI framework.
|||
||| This framework provides principled support for common patterns of
||| interaction, e.g.:
||| - input modes
||| - command lines
||| - tool and menu bars
||| - structural editing
|||
||| For TUI applications, input events mainly consist of
||| keystrokes. Modal is a framework for handling more complex
||| patterns of key-driven interaction. Think: command lines, repls,
||| and structural editors.
|||
||| Modal is based on the same automata theory used in lexing and
||| parsing, but with a different emphasis: For example, there is no
||| need to track source locations, and moreover there is a bias
||| toward merely reporting invalid input and moving on, vs exiting
||| the process with a status code. There is less focus on formal
||| grammars, and more focus on providing sensible choices that are
||| convenient to the user. Similarly, there's no need to avoid
||| left-recursion: control always passes back to the user after each
||| input event.

module TUI.Modal


import Data.Maybe
import Data.SortedMap
import Derive.Prelude
import TUI.View


%default total
%language ElabReflection

||| The output of a state machine's transition function.
|||
||| @Discard Accept the current token, but otherwise do nothing.
||| @Advance Transition to the next state, emitting the given action.
||| @Accept  Transition to the final state, possibly emitting a final action.
||| @Reject
public export
data Step stateT outputT
  = Discard
  | Advance stateT  (Maybe outputT)
  | Accept  outputT
  | Reject

||| This interface abstracts over finite state machines.
public export
interface Automata stateT inputT outputT | stateT where
  ||| Reset this machine to its initial state.
  reset : stateT -> stateT
  ||| Feed one token into this machine.
  step : inputT -> stateT -> Step stateT outputT

  ||| A user-visible string identifying the current mode.
  mode : stateT -> String
  mode = const ""

  ||| help screen
  legend : stateT -> List (inputT, String)
  legend = const []

||| XXX: Dubious
export
Functor (Step stateT) where
  map f Discard       = Discard
  map f (Advance x y) = Advance x (f <$> y)
  map f (Accept x)    = Accept (f x)
  map f Reject        = Reject

||| Lift inner response to outer machine type
lift
  : Automata innerT inputT outputT
  => inputT
  -> innerT
  -> (innerT -> outerT)
  -> Step outerT outputT
lift input self f = impl $ step input self
  where
    impl : Step innerT outputT -> Step outerT outputT
    impl Discard       = Discard
    impl (Advance x y) = Advance (f x) y
    impl (Accept x)    = Accept x
    impl Reject        = Reject

||| Fixed mapping from input to output
namespace Mapping

  ||| A binding from input symbol to output action.
  public export
  record Binding inputT outputT where
    constructor Bind
    input     : inputT
    output    : outputT
    docstring : String

  ||| A mapping from input symbols to output actions.
  export
  0 Mapping : Type -> Type -> Type
  Mapping inputT outputT = SortedMap inputT (Binding inputT outputT)

  export
  Automata (Mapping inputT outputT) inputT outputT where
    reset = id

    step input self = case lookup input self of
      Just binding => Advance self (Just binding.output)
      Nothing      => Discard

  ||| Construct a mapping from a list of bindings
  export
  mapping : Ord inputT => List (Binding inputT outputT) -> Mapping inputT outputT
  mapping bindings = fromList $ mapBinding <$> bindings
    where
      mapBinding : Binding inputT outputT -> (inputT, Binding inputT outputT)
      mapBinding self = (self.input, self)

||| Sequence state machines with compatible types.
namespace Seq
  ||| Sequence two machines together with compatible types.
  export
  record Seq a b where
    constructor MkSeq
    left  : a
    right : b
    state : Maybe (Either a b)

  ||| Run the left automata until it ends, then run the right machine.
  export
  implementation
       Automata a inputT outputT
    => Automata b inputT outputT
    => Automata (Seq a b) inputT outputT
  where
    reset self = {state := Just $ Left self.left} self

    step input self = case self.state of
      Nothing => Reject
      Just (Left  left) => case lift input left updateLeft of
        Accept output => Advance
          ({state := Just $ Right self.right} self)
          (Just output)
        passthrough => passthrough
      Just (Right right) => lift input right updateRight
    where
      updateLeft : a -> Seq a b
      updateLeft left = {state := Just $ Left left} self

      updateRight : b -> Seq a b
      updateRight right = {state := Just $ Right right} self

  export
  (::) : a -> b -> Seq a b
  a :: b = MkSeq a b $ Just $ Left a

||| Descend into right or left automata based on initial input.
namespace Alt
  export
  record Alt a b inputT where
    constructor MkAlt
    choose : inputT -> Either a b
    state  : Maybe $ Either a b

  export
  implementation
       Automata a inputT outputT
    => Automata b inputT outputT
    => Automata (Alt a b inputT) inputT outputT
  where
    reset self = {state := Nothing} self

    step input self = case self.state of
      Nothing => Advance ({state := Just $ self.choose input} self) Nothing
      Just (Left  left)  => lift input left updateLeft
      Just (Right right) => lift input right updateRight
    where
      updateLeft : a -> Alt a b inputT
      updateLeft left = {state := Just $ Left left} self

      updateRight : b -> Alt a b inputT
      updateRight right = {state := Just $ Right right} self

  export
  alt : (inputT -> Either a b) -> Alt a b inputT
  alt choose = MkAlt choose Nothing

||| Switch between two automata based on a pair of predicates
namespace Switch
  export
  record Switch a b inputT where
    switchLeft  : inputT -> Bool
    switchRight : inputT -> Bool
    left  : a
    right : b
    state : Either a b

  export
  implementation
       Automata a inputT outputT
    => Automata b inputT outputT
    => Automata (Alt a b inputT) inputT outputT
  where
    reset self = {state := Nothing} self

    step input self = case self.state of
      Nothing => Advance ({state := Just $ self.choose input} self) Nothing
      Just (Left  left)  => lift input left updateLeft
      Just (Right right) => lift input right updateRight
    where
      updateLeft : a -> Alt a b inputT
      updateLeft left = {state := Just $ Left left} self

      updateRight : b -> Alt a b inputT
      updateRight right = {state := Just $ Right right} self

  export
  implementation
       Automata a inputT outputT
    => Automata b inputT outputT
    => Automata (Switch a b inputT) inputT outputT
  where
    reset self = case self.state of
      Left  state => {state := Left  $ reset state} self
      Right state => {state := Right $ reset state} self

    step input self = case self.state of
      Left state => case self.switchRight input of
        False => lift input state updateLeft
        True  => Advance ({state := Right self.right, left := state} self) Nothing
      Right state => case self.switchLeft input of
        False => lift input state updateRight
        True  => Advance ({state := Left self.left, right := state} self) Nothing
    where
      updateLeft : a -> Switch a b inputT
      updateRight : b -> Switch a b inputT

namespace Until
  ||| Run the given automata until we see a particular symbol.
  export
  record Loop stateT inputT where
    constructor MkLoop
    predicate : inputT -> Bool
    state     : stateT

  export
  implementation
       Automata stateT inputT outputT
    => Automata (Loop stateT inputT) inputT outputT
  where
    reset self = {state $= reset} self

    step input self = case self.predicate input of
      True => lift input self.state update
      False => case lift input self.state update of
        Discard                 => Reject
        Advance _ (Just action) => Accept action
        Advance _ Nothing       => Reject
        passthrough             => passthrough
    where
      update : stateT -> Loop stateT inputT
      update state = {state := state} self

  ||| Run the given automata until the predicate matches the input.
  export
  until : (inputT -> Bool) -> stateT -> Loop stateT inputT
  until predicate state = MkLoop predicate state

  ||| Run the given automata while the predicate matches the input.
  export
  while : (inputT -> Bool) -> stateT -> Loop stateT inputT
  while predicate state = MkLoop (not . predicate) state

namespace Retry
  ||| Restart the given automata when it accepts or rejects.
  export
  record Retry stateT where
    constructor MkRetry
    state : stateT

  reset : Automata stateT inputT outputT => Retry stateT -> Retry stateT
  reset self = {state $= reset} self

  export
  implementation
       Automata stateT inputT outputT
    => Automata (Retry stateT) inputT outputT
  where
    reset = reset

    step input self = case lift input self.state update of
      Reject        => Advance (reset self) Nothing
      Accept output => Advance (reset self) (Just output)
      passthrough   => passthrough
    where
      update : stateT -> Retry stateT
      update state = {state := state} self

  ||| Restart the given automata when it accepts or rejects.
  export
  retry : Automata s i o => s -> Retry s
  retry init = MkRetry init

namespace Filter
  ||| Filter symbols from the input stream by the given predicate
  export
  record Filter stateT inputT where
    constructor MkFilter
    predicate : inputT -> Bool
    state : stateT

  reset
    :  Automata stateT inputT outputT
    => Filter stateT inputT
    -> Filter stateT inputT
  reset self = {state $= reset} self

  export
  implementation
       Automata stateT inputT outputT
    => Automata (Filter stateT inputT) inputT outputT
  where
    reset = reset

    step input self = case self.predicate input of
      False => Discard
      True  => lift input self.state update
    where
      update : stateT -> Filter stateT inputT
      update state = {state := state} self

  ||| Filter symbols from the input stream by the given predicate
  filter
    :  Automata stateT inputT actionT
    => (inputT -> Bool)
    -> stateT
    -> Filter stateT inputT
  filter predicate init = MkFilter predicate init


||| Dynamic dispatch over state machines (used in implementations)
namespace Dynamic
  record Dynamic inputT outputT where
    constructor Dyn
    {0 stateT : Type}
    {auto impl : Automata stateT inputT outputT}
    state : stateT

  setState
    :  (self : Dynamic inputT outputT)
    -> self.stateT
    -> Dynamic inputT outputT
  setState self state = {state := state} self

  Automata (Dynamic inputT outputT) inputT outputT where
    reset self = {state $= reset @{self.impl}} self
    step input self = lift @{self.impl} input self.state (setState self)

||| Abstract over escape sequences.
namespace Escape
  public export
  data Class
    = Begin
    | Capture
    | Ignore
    | End

  export
  record Buffer inputT outputT where
    constructor MkBuffer
    validate : SnocList inputT -> Maybe outputT
    done     : inputT -> Bool
    state    : SnocList inputT

  public export
  record Rule inputT outputT where
    constructor R
    classify : inputT -> Class
    match    : SnocList inputT -> Maybe outputT

  export
  buffer : Rule inputT outputT -> Buffer inputT outputT
  buffer rule = MkBuffer rule.match isEnd [<]
  where
    isEnd : inputT -> Bool
    isEnd input = case rule.classify input of
      End => True
      _   => False

  export
  Automata (Buffer inputT outputT) inputT outputT where
    reset = {state := [<]}

    step input self = case self.done input of
      False => Advance ({state $= (:< input)} self) Nothing
      True  => fromMaybe Reject $ Accept <$> self.validate self.state

  ||| Wraps an inner state-machine.
  |||
  ||| In the default state, merely passes input through to the inner machine.
  ||| If we detect the start of a sequence, transitions to escaped state.
  ||| In escaped state, buffers input until we detect the end of a sequence.
  ||| At the end of a sequence, if input yields an action, we emit
  ||| it. Otherwise, resets to default.
  export
  record Escaped stateT inputT outputT where
    constructor MkEscaped
    rule     : Rule inputT outputT
    state    : stateT
    escaped  : Maybe (Buffer inputT outputT)

{-
  export
  implementation
       Automata stateT inputT outputT
    => Automata (Escaped stateT inputT outputT) inputT outputT
  where
    reset = {escaped := Nothing, state $= reset}

    step input self = case self.escaped of
      Nothing => case self.rule.classify input of
        Begin => Advance toEscaped Nothing
        _     => lift input self.state updateState
      Just buf => case lift input buf updateBuf of
        Accept output => Advance toDefault (Just output)
        passthrough   => passthrough
    where
      updateState : stateT -> Escaped stateT inputT outputT
      updateState state = {state := state} self

      updateBuf : Buffer inputT outputT -> Escaped stateT inputT outputT
      updateBuf buffer = {escaped := Just buffer} self

      toEscaped : Escaped stateT inputT outputT
      toEscaped = {escaped := Just (buffer self.rule)} self

      toDefault : Escaped stateT inputT outputT
      toDefault = {escaped := Nothing} self

  export
  escaped
    : Automata stateT inputT outputT
    => Rule inputT outputT
    -> stateT
    -> Escaped stateT inputT outputT
  escaped rule init = MkEscaped rule init Nothing


namespace PostfixInterpreter

  export
  record Stack stateT parserT valueT commandT where
    state  : stateT
    parser : parserT
    stack  : SnocList valueT

  implementation
       Automata stateT  inputT commandT
    => Automata parserT inputT (Either valueT (commandT, SnocList valueT))
    => Automata (Stack stateT parserT valueT) inputT commandT
  where
    reset = ?resethole
    step input self = case lift input self.parser updateParser of
      Accept (Left  value) => Advance (push value) Nothing
      Accept (Right command) => Advance
    where
      updateParser : parserT -> Stack stateT parserT valueT
