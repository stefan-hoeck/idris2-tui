||| Minimalist terminal UI framework.
|||
||| This module provides partial support for decoding ansi input
||| escape sequences into high-level keys. This is the kind of thing
||| that curses would do for us, but we have to do ourselves.
module TUI.Event


import Data.IORef
import Derive.Prelude
import JSON.Derive
import System
import System.Concurrency
import Util


%language ElabReflection
%default total


||| This is a pure Queue data-structure more or less copied from
||| idris2-containers
|||
||| It's used internally to store the FIFO data.
record Queue valueT where
  constructor Q
  front : List valueT
  back  : SnocList valueT

empty : Queue valueT
empty = Q [] [<]

enqueue : valueT -> Queue valueT -> Queue valueT
enqueue v = { back $= (:< v) }

dequeue : Queue valueT -> Maybe (valueT, Queue valueT)
dequeue self = case self.front of
  [] => case self.back of
    [<] => Nothing
    xs :< x  => Just $ (x, Q (toList xs) [<])
  x :: xs => Just (x, { front := xs } self)


||| Like System.Concurrency.Channel, but:
|||
||| -- has non-blocking API
||| -- hold values until dequeued.
export
record Fifo valueT where
  constructor MkFifo
  mutex    : Mutex
  queue    : IORef (Queue valueT)

export
makeFifo : IO (Fifo valueT)
makeFifo = do
  mutex  <- makeMutex
  queue  <- newIORef empty
  pure $ MkFifo mutex queue

||| Put a value into the fifo.
|||
||| This should not block for long, as the queue is unbounded. It may
||| exhaust memory if the queue isn't being drained.
export
put : Fifo valueT -> valueT -> IO ()
put self value = do
  mutexAcquire self.mutex
  modifyIORef self.queue (enqueue value)
  mutexRelease self.mutex

||| Get the next item from the queue without blocking.
|||
||| If the queue is empty, immediately returns Nothing.
export covering
get' : Fifo valueT -> IO (Maybe valueT)
get' self = do
  mutexAcquire self.mutex
  q <- readIORef self.queue
  case dequeue q of
    Nothing => do
      mutexRelease self.mutex
      pure Nothing
    Just (a, q) => do
      writeIORef self.queue q
      mutexRelease self.mutex
      pure $ Just a


||| Decoder state-machine type.
|||
||| XXX: I don't like my approach here, it's clunky.
export
data EscState state
  = HaveEsc (SnocList Char) state
  | Default state

||| Abstract key value, decoded from ANSI escape sequence.
public export
data Key
  = Alpha Char
  | Left
  | Right
  | Up
  | Down
  | Delete
  | Enter
  | Tab
  | Escape
%runElab derive "Key" [Ord, Eq, Show, FromJSON]

||| Track escape sequences for the inner state.
export
wrapEsc : state -> EscState state
wrapEsc = Default

||| Project the wrapped value from the escape sequence state.
export
unwrapEsc : EscState state -> state
unwrapEsc (HaveEsc _ s) = s
unwrapEsc (Default   s) = s

||| It's handy to be able to `show` the escape state for debugging.
export
Show s => Show (EscState s) where
  show = show . unwrapEsc

||| Decode well-known escape sequences into abstract keys.
|||
||| In particular, we handle the cursor keys.
|||
||| This is just a quick-and-dirty MVP. Just the cursor keys and
||| escape. No attempt to decode modifiers.
|||
||| XXX: do something clever here to make supporting the whole spec
||| easier.
export
decodeEsc : SnocList Char -> Maybe (Maybe Key)
decodeEsc [<]          = Just Nothing
decodeEsc [< '[']      = Just Nothing
decodeEsc [< '[', 'C'] = Just $ Just Right
decodeEsc [< '[', 'D'] = Just $ Just Left
decodeEsc [< '[', 'A'] = Just $ Just Up
decodeEsc [< '[', 'B'] = Just $ Just Down
decodeEsc [< '\ESC']   = Just $ Just Escape
decodeEsc _            = Nothing

||| Interpret console escape sequences as keys.
|||
||| Note: this falls down if the user presses the escape key, because
||| we can't tell the difference between the key press and the start
||| of an escape sequence. It'd be better to use ncurses.
export
interpretEsc
  : Monad m
  =>  (Key -> stateT -> m (Maybe stateT))
  -> Char
  -> EscState stateT
  -> m (Maybe (EscState stateT))
interpretEsc f c (HaveEsc esc s) = case (decodeEsc $ esc :< c) of
  Just Nothing    => pure $ Just $ HaveEsc (esc :< c) s
  Just (Just key) => pure $ map Default !(f key s)
  Nothing         => pure $ Just $ Default s
interpretEsc f '\ESC' (Default s) = pure $ Just $ HaveEsc [<] s
interpretEsc f '\DEL' (Default s) = pure $ map Default !(f Delete    s)
interpretEsc f '\n'   (Default s) = pure $ map Default !(f Enter     s)
interpretEsc f '\t'   (Default s) = pure $ map Default !(f Tab       s)
interpretEsc f c      (Default s) = pure $ map Default !(f (Alpha c) s)

export
mapEsc
  :  Monad m
  => (eventT -> stateT -> m (Maybe stateT))
  -> eventT
  -> EscState stateT
  -> m (Maybe (EscState stateT))
mapEsc handler event (HaveEsc esc s) = pure $ map (HaveEsc esc) !(handler event s)
mapEsc handler event (Default s)     = pure $ map Default $ !(handler event s)
