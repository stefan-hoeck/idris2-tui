-- BSD 3-Clause License
--
-- Copyright (c) 2023, Brandon Lewis
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

||| Entry point for running TUI applications.
|||
||| The code in this file takes care of initializing and restoring
||| terminal state, and then begins running the desired top-level
||| code.

module TUI.MainLoop

import Data.List.Quantifiers
import Data.List.Quantifiers.Extra
import Control.ANSI
import System
import System.File
import TUI.Event
import TUI.Painting
import TUI.Component
import TUI.Component.Modal


%default total


{- These routines should probably be contributed to the ANSI library -}


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
||| For now this is the only way to use this library, but I also see
||| this as a useful mechanism for testing and debugging.
namespace InputShim
  ||| Initialize the terminal, then enter the main loop.
  |||
  ||| XXX: Run with python input shim in this directory.
  |||
  ||| @ sources  A list of event sources.
  ||| @ handlers A list of handlers covering each event source.
  ||| @ render   A function to render the current state to the screen.
  ||| @ init     The initial application state.
  |||
  ||| This is the lowest-level entry point.
  export covering
  runRaw
    :  (sources  : All Event tys)
    -> (handlers : All (Event.Handler stateT valueT) tys)
    -> (render   : stateT  -> Context ())
    -> (init     : stateT)
    -> IO (Maybe valueT)
  runRaw sources handlers render init = do
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
      case !(decodeNext next sources) of
        Right event => case !(handleEvent event state handlers) of
          Left  next => loop next
          Right res  => pure res
        Left err     => do
          ignore $ fPutStrLn stderr $ show err
          loop state


namespace MVC
  ||| Like runRaw, but for `stateT` which implement `View`.
  |||
  ||| Accordingly, it drops the `render` parameter.
  export covering
  runView
    :  View stateT
    => (sources : All Event tys)
    -> (handlers : All (Event.Handler stateT valueT) tys)
    -> stateT
    -> IO (Maybe valueT)
  runView sources handlers init =
    runRaw
      sources
      handlers
      (View.paint Focused !(screen))
      init

  ||| Like runView, but for `Component`.
  |||
  ||| Right now this hard codes the set of event sources and handlers.
  export covering
  runComponent
    :  (self    : Component valueT)
    -> IO (Maybe valueT)
  runComponent self = runView [!onAnsiKey] [handle] (root self)
