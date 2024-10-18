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
import TUI.Component.Modal


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
  ||| @ init     The initial application state.
  |||
  ||| This is the lowest-level entry point. Use this if you don't want
  ||| escape sequence decoding, or any higher-level abstractions.
  |||
  ||| The `update` function should return `Left` if processing should
  ||| continue, or `Right` to end computation with the final value.
  export covering
  runRaw
    :  (sources : List (Event stateT valueT))
    -> (render  : stateT  -> Context ())
    -> (init    : stateT)
    -> IO (Maybe valueT)
  runRaw sources render init = do
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
    loop : stateT -> IO (Maybe valueT)
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
      case decodeNext next sources of
        Right handler => case !(handler state) of
          Left  next => loop next
          Right res  => pure res
        Left err     => do
          ignore $ fPutStrLn stderr $ show err
          loop state

  ||| Like runRaw, but handles decoding ANSI escape sequences into
  ||| high-level key events.
  |||
  ||| Use this entry point when you want escape sequence decoding, but
  ||| do not want to use the `View` or `Component` abstractions.
  |||
  ||| The `onKey` handler you supply should update the global
  ||| application state in response to the given key press.
  export covering
  runTUI
    :  (onKey   : Event.Handler stateT valueT Key)
    -> (sources : List (Event stateT valueT))
    -> (render  : stateT -> Context ())
    -> (init    : stateT)
    -> IO (Maybe valueT)
  runTUI onKey sources render init =
    runRaw
      ((onAnsiKey onKey) :: (liftEsc <$> sources))
      (render . unwrap)
      (wrap init)


namespace MVC
  ||| Like runTUI, but for `stateT` which implement `View`.
  |||
  ||| Accordingly, there is no need to supply an explicit `render`
  ||| function.
  export covering
  runView
    :  View stateT
    => (onKey : Event.Handler stateT valueT Key)
    -> (sources : List (Event stateT valueT))
    -> stateT
    -> IO (Maybe valueT)
  runView onKey sources init =
    runTUI
      onKey
      sources
      (View.paint Focused !(screen))
      init

  ||| Like runView, but we expect a `Response` instead of a `Result`.
  |||
  ||| This is an implementation detail for `runModal`.
  export covering
  runMVC
    :  View stateT
    => (onKey : Event.Handler stateT valueT Key)
    -> (sources : List (Event stateT valueT))
    -> stateT
    -> IO (Maybe valueT)
  runMVC onKey sources init = runView onKey [] init

  ||| Like runView, but for `Component` views, which know how to
  ||| handle events on their own.
  |||
  ||| XXX: components can only handle Key events, I need to fix that.
  export covering
  runComponent : Component valueT -> IO (Maybe valueT)
  runComponent self = runMVC handle [] (root self)
