||| Minimalist terminal UI framework.
|||
||| Entry points for running TUI applications.

module TUI.MainLoop

import Data.Vect
import Data.Vect.Quantifiers
import JSON
import System
import System.File
import System.Signal
import TUI.View
import Util


%default total


||| This is the original mainloop implementation which directly reads
||| from STDIN and directly communicates with the HID scale.
|||
||| It doesn't work.
|||
||| The issue seems to be synchronization issues with using a single
||| channel with more than two threads. Idris' standard library
||| doesn't have a proper fifo for synchronizing between multiple
||| producers / consumers.
|||
||| Even if we had it, though, there would still be the HTTP upload
||| piece. tyttp is an option, however, we don't have library support
||| for multi-part mime decoding, so this would still have to be
||| handled externally.
|||
||| I'm leaving this here so we can hopefully get back to it, once all
||| the right pieces are in place.
namespace PureIdris
  ||| Low-level TUI application mainloop.
  |||
  ||| Manages terminal state, handles OS-level signals, and receives
  ||| keyboard events from STDIN.
  |||
  ||| Use this entry point if you do not want escape-sequence decoding.
  |||
  ||| SIGINT is interpreted as *Cancel*, meaning the initial state is
  ||| returned, discarding the user's changes.
  |||
  ||| Handler is called to process input events. It main job is to
  ||| compute the next state, but it runs in IO so that your program can
  ||| take arbitrary actions in response to user input.
  export covering
  runRaw
    :  (handler : Char -> stateT -> IO (Maybe stateT))
    -> (render  : stateT -> IO Builtin.Unit)
    -> (init    : stateT)
    -> IO stateT
  runRaw handler render init = do
    -- default SigINT handler doesn't clean up raw mode, so we need to
    -- handle it explicitly and make sure to clean up.
    Right _ <- collectSignal SigINT
             | Left err => die "couldn't trap SigINT"

    -- run mainloop
    altScreen True
    hideCursor
    saveCursor
    ret <- withRawMode err (loop init)
    cleanup
    pure ret
  where
    -- restore terminal state as best we can
    cleanup : IO ()
    cleanup = do
      clearScreen
      restoreCursor
      showCursor
      altScreen False

    -- ensure we restore terminal state on IO error
    err : e -> IO stateT
    err _ = do
      cleanup
      die "an unhandled error occured"

    -- this is the actual recursive mainloop. The unusual `()` in the
    -- signature allows loop to be partially-applied above.
    loop : stateT -> () -> IO stateT
    loop s () = do
      -- repaint the screen with the current state
      clearScreen
      moveTo (MkPos 0 0)
      render s

      -- Return immediately if SigINT was received. Nothing, in this
      -- case, means no signal, so continue normal operation.
      --
      -- If we ever need to handle some other signal, like SIGWINCH,
      -- it would be done here.
      Nothing <- handleNextCollectedSignal
               | Just SigINT => pure init
               | Just _      => die "unexpected signal"

      -- handle next key press
      case !(handler !getChar s) of
        Nothing   => pure s -- we are done, quit
        Just next => loop next () -- go to next iteration.

  ||| Run a raw TUI application, decoding input escape sequences.
  |||
  ||| Use this function if you want escape sequence decoding, but do not
  ||| want to use the view abstraction for rendering screen contents.
  covering export
  runTUI
    :  (handler : Key -> stateT -> IO (Maybe stateT))
    -> (render  : stateT -> IO ())
    -> (init    : stateT)
    -> IO stateT
  runTUI handler render init = do
    ret <- runRaw (interpretEsc handler) (render . unwrapEsc) (wrapEsc init)
    pure $ unwrapEsc ret

  ||| Run a top-level View.
  |||
  ||| Use this entry point if you want to use the `View` abstraction.
  |||
  ||| This will run until the top-level view gives up its focus.
  covering export
  runView
    : View stateT actionT
    => (handler : actionT -> stateT -> IO stateT)
    -> (init : stateT)
    -> IO stateT
  runView handler init = do
    result <- runTUI wrapView update init
    pure result
  where
    update : stateT -> IO ()
    update state = do
      beginSyncUpdate
      paint Focused !(screen) state
      endSyncUpdate

    wrapView : Key -> stateT -> IO (Maybe stateT)
    wrapView k s = case handle k s of
      Update s    => pure $ Just s
      FocusParent => pure Nothing
      FocusNext   => pure $ Just s
      Run action  => pure $ Just !(handler action s)


||| This is the current mainloop stack.
|||
||| It reads line-separated JSON records from stdin. So a lot of the
||| tricky stuff is done in python.
|||
||| This is temporary, until the right functionality is available
||| directly in idris, either via FFI and custom build hooks, or via
||| upstream contributions to the idris ecosystem.
namespace InputShim
  ||| Pair an event that can be decoded from json with its handler.
  public export
  record EventSource stateT where
    constructor On
    tag         : String
    {auto impl  : FromJSON eventT}
    handler     : eventT -> stateT -> IO (Maybe stateT)

  ||| Decode the top-level JSON record.
  |||
  ||| This is somewhat manual, since there's no outer Sum type to
  ||| decode into. We directly decode the first element of `contents`,
  ||| which is where our event type actually lives.
  |||
  ||| XXX: Probably the JSON package has this functionality, but I
  ||| didn't want to go down that rabbit hole, so I wrote it manually.
  export
  match : FromJSON a => String -> JSON -> Either String a
  match expected (JObject [
    ("tag", JString got),
    ("contents", (JArray [rest]))
  ]) =
    if expected == got
      then case fromJSON rest of
        Left err => Left $ show err
        Right v  => Right v
      else Left "Incorrect tag"
  match _ _ = Left "Wrong shape"

  ||| Try each handler in succession until one decodes an event.
  |||
  ||| Returns the original handler, with the event partially applied
  ||| (which avoids having to mention it in the return type).
  export
  decodeNext
    : String
    -> List (EventSource stateT)
    -> Either String (stateT -> IO (Maybe stateT))
  decodeNext e sources = case parseJSON Virtual e of
      Left  err => Left "Parse Error: \{show err}"
      Right parsed => loop parsed sources
  where
    loop
      : JSON
      -> List (EventSource stateT)
      -> Either String (stateT -> IO (Maybe stateT))
    loop parsed []        = Left "Unhandled event: \{e}"
    loop parsed (x :: xs) = case match @{x.impl} x.tag parsed of
        Left  err => loop parsed xs
        Right evt => Right (x.handler evt)

  ||| This reads JSON packets over stdin, one per line.
  |||
  ||| Run with python shim in this directory.
  export
  covering
  runRaw
    :  (sources : List (EventSource stateT))
    -> (render  : stateT -> IO Builtin.Unit)
    -> (init    : stateT)
    -> IO stateT
  runRaw sources render init = do
    -- input-shim.py already put the terminal in raw mode
    putStrLn ""
    altScreen True
    hideCursor
    saveCursor
    ret <- loop init
    cleanup
    pure ret
  where
    ||| restore terminal state as best we can
    cleanup : IO Builtin.Unit
    cleanup = do
      clearScreen
      restoreCursor
      showCursor
      altScreen False

    ||| The actual main loop
    loop: stateT -> IO stateT
    loop state = do
      beginSyncUpdate
      clearScreen
      moveTo origin
      render state
      endSyncUpdate

      next <- getLine
      next' <- case decodeNext next sources of
        Right handler => handler state
        Left  err     => do
          ignore $ fPutStrLn stderr $ show err
          pure (Just state)
      case (the (Maybe stateT) next') of
        Nothing => pure state
        Just next => loop next

  ||| Run a raw TUI application, decoding input escape sequences.
  |||
  ||| Use this function if you want escape sequence decoding, but do not
  ||| want to use the view abstraction for rendering screen contents.
  covering export
  runTUI
    :  (onKey    : (Key -> stateT -> IO (Maybe stateT)))
    -> (sources : List (EventSource stateT))
    -> (render  : stateT -> IO Builtin.Unit)
    -> (init    : stateT)
    -> IO stateT
  runTUI onKey sources render init = do
    ret <- runRaw
      (On "Stdin" (interpretEsc onKey) :: (map {handler $= mapEsc} sources))
      (render . unwrapEsc)
      (wrapEsc init)
    pure $ unwrapEsc ret

  ||| Run a top-level View.
  |||
  ||| Use this entry point if you want to use the `View` abstraction.
  |||
  ||| This will run until the top-level view gives up focus.
  covering export
  runView
    : View stateT actionT
    => (onAction : actionT -> stateT -> IO stateT)
    -> (sources : List (EventSource stateT))
    -> (init : stateT)
    -> IO stateT
  runView onAction sources init = do
    runTUI wrapView sources (paint Focused !(screen)) init
  where
    wrapView : Key -> stateT -> IO (Maybe stateT)
    wrapView k s = case handle k s of
      Update s    => pure $ Just s
      FocusParent => pure Nothing
      FocusNext   => pure $ Just s
      Run action  => pure $ Just $ !(onAction action s)
