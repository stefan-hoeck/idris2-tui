||| Minimalist terminal UI framework.
|||
||| Entry point for running TUI applications.
|||
||| The code in this file takes care of initializing and restoring
||| terminal state, and then begins running the desired top-level
||| code.

module TUI.MainLoop

import Control.ANSI
import System
import System.File
import TUI.Event
import TUI.Painting
import TUI.Component
import TUI.Component.Stack


%default total


||| Clear the contents of the screen.
clearScreen : IO ()
clearScreen = putStr $ eraseScreen All

||| Switch into or out of the alternate screen buffer
altScreen : Bool -> IO ()
altScreen True  = putStr $ "\ESC[?1049h"
altScreen False = putStr $ "\ESC[?1049l"

||| Show or hide cursor
cursor : Bool -> IO ()
cursor True  = putStr "\ESC[?25h"
cursor False = putStr "\ESC[?25l"

||| Tell the terminal to save its state.
saveCursor : IO ()
saveCursor = putStr "\ESC7"

||| Tell the terminal to restore its state.
restoreCursor : IO ()
restoreCursor = putStr "\ESC8"

||| synchronous update Supported by iTerm2 and other fancy terminals
beginSyncUpdate : IO ()
beginSyncUpdate = putStrLn "\ESC[?2026h"

||| synchronous update supported by iTerm2 and other fancy terminals
endSyncUpdate : IO ()
endSyncUpdate = putStrLn "\ESC[?2026l"


||| This is the current mainloop stack.
|||
||| It reads line-separated JSON records from stdin. So a lot of the
||| tricky stuff is done in python.
|||
||| This is temporary, until the right functionality is available in
||| idris.
namespace InputShim
  ||| Initialize the terminal, then enter the main loop.
  |||
  ||| XXX: Run with python shim in this directory.
  |||
  ||| @ sources  A list of event sources.
  ||| @ render   A function to render the current state to the screen.
  ||| @ update   A function to run in response to an action.
  ||| @ init     The initial application state.
  |||
  ||| This is the lowest-level entry point. Use this if you don't want
  ||| escape sequence decoding, or any higher-level abstractions.
  |||
  ||| The `update` function should return `Left` if processing should
  ||| continue, or `Right` to end computation with the final value.
  export
  covering
  runRaw
    :  (sources : List (EventSource stateT actionT))
    -> (render  : stateT  -> Context Builtin.Unit)
    -> (update  : actionT -> stateT -> IO (Either stateT valueT))
    -> (init    : stateT)
    -> IO valueT
  runRaw sources render update init = do
    -- input-shim.py already put the terminal in raw mode
    putStrLn ""
    altScreen True
    cursor False
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
      cursor True
      altScreen False

    ||| The actual main loop
    loop: stateT -> IO valueT
    loop state = do
      beginSyncUpdate
      clearScreen
      present $ do
        -- all drawing operations now live in the `Context` monad,
        -- so they must be nested under the `present` IO action.
        moveTo origin
        render state
      endSyncUpdate
      fflush stdout

      next <- getLine
      next' <- case decodeNext next sources of
        Right handler => update (handler state) state
        Left  err     => do
          ignore $ fPutStrLn stderr $ show err
          pure $ Left state
      case next' of
        Left  state => loop state
        Right value => pure value

  ||| Run a TUI application, decoding input escape sequences.
  |||
  ||| Use this function if you want escape sequence decoding, but do not
  ||| want to use the MVC abstractions.
  covering export
  runTUI
    :  (onKey   : (Key -> stateT -> actionT))
    -> (sources : List (EventSource stateT actionT))
    -> (render  : stateT -> Context Builtin.Unit)
    -> (update  : actionT -> stateT -> IO (Either stateT valueT))
    -> (init    : stateT)
    -> IO valueT
  runTUI onKey sources render update init =
    runRaw
      (On "Stdin" (handleEsc onKey) :: (liftEsc <$> sources))
      (render . unwrap)
      (updateEsc update)
      (wrap init)


namespace MVC
  covering
  runView
    :  View stateT
    => (sources : List (EventSource stateT (Response stateT valueT)))
    -> stateT
    -> (Key -> stateT -> Response stateT valueT)
    -> IO (Maybe valueT)
  runView sources init handler =
    runTUI
      handler
      sources
      (View.paint Focused !(screen))
      liftUpdate
      init
  where
    ||| Lift a Response to an (IO Result).
    |||
    ||| Essentially, this interprets the `Response` DSL keywords. It is
    ||| called by runComponent in MainLoop.idr.
    liftUpdate
      :  Response stateT valueT
      -> stateT
      -> IO (Result stateT valueT)
    liftUpdate Ignore     self = pure $ Left self
    liftUpdate (Yield x)  _    = pure $ Right x
    liftUpdate (Do next)  _    = pure $ Left next
    liftUpdate (Run next) _    = pure $ Left !next
    liftUpdate (Push t m) self = assert_total $ idris_crash "unhandled Push"

  ||| Run the given component UI.
  |||
  ||| Use this entry point if your top-level state implements the
  ||| component interface.
  export covering
  runComponent
    :  (self : Component valueT)
    -> (sources : List (EventSource self.State (Response self.State valueT)))
    -> IO (Maybe valueT)
  runComponent self sources = runView [] (modal $ root self) handle
