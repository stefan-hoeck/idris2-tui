||| Driver for talking to 510 Scale over USB.
|||
||| The scale sends a 6-byte packet, which must be manually swizzled
||| to decode the weight.
module USBScale


import Barcode
import Control.ANSI
import Data.Bits
import Data.Buffer
import Data.Either
import Data.Vect
import System.Concurrency
import System
import System.File
import System.Signal
import TUI
import TUI.Event
import TUI.View
import TUI.Painting
import Util

import Derive.Prelude

import Measures


%language ElabReflection
%default total


||| Move to Utils lib
debug : Show a => a -> IO Builtin.Unit
debug value = do
  _ <- fPutStrLn stderr (show value)
  pure ()

||| Unit to use when we can't determine from the USB traffic
defaultUnit : Unit Mass
defaultUnit = Grams

||| Convert integer unit into type
units : Bits8 -> Unit Mass
units 1  = MilliGrams
units 2  = Grams
units 3  = KiloGrams
units 11 = Ounces
units 12 = Pounds
units _  = defaultUnit

||| Result of scale IO operation
public export
data Result
  = Empty
  | Weighing
  | Fault String
  | Ok Weight
%runElab derive "Result" [Show,Eq]


||| Calculate the current weight from the raw binary values
calcWeight : Bits8 -> Int -> Int -> Int -> Weight
calcWeight unit msb lsb exponent =
  let
    mantissa = cast {to = Double} ((msb `shiftL` 8) .|. lsb)
    unit   = units unit
    scaled = case unit of
      Ounces => mantissa / 10.0
      _      => mantissa
  in case exponent of
      0x00 => Q (cast scaled) unit
      0xff => Q (cast scaled) unit
      _    => Q (pow scaled (cast exponent)) unit

||| Decode the 6-byte HID packet into a scale value
decode : List Bits8 -> Result
decode [report, status, unit, exp, lsb, msb] =
  if report == 0x03
    then case status of
      0x01 => Fault "Fault"
      0x02 => Empty
      0x03 => Weighing
      0x04 => Ok $ calcWeight unit (cast msb) (cast lsb) (cast exp)
      0x05 => Fault "Negative Weight"
      0x06 => Fault "Overweight"
      0x07 => Fault "Recalibrate"
      0x08 => Fault "Rezero"
      _    => Fault $ "Unknown status code: " ++ show status
    else Fault "Error Reading Scale!"
decode _ = Fault"Error, invalid packet"


||| Synchronously read from the scale
|||
||| We can't necessarily trust the first weight we get, so we wait for
||| a few valid weights to come in before returning the value.
export partial
getWeight : String -> IO (Either String Weight)
getWeight path = do
  Just buf <- newBuffer 6 | Nothing => pure $ Left "Couldn't allocate buffer"
  withFile path Read onError (loopN buf 5 Nothing)
  where
    loopN : Buffer -> Nat -> Maybe Weight -> File -> IO (Either String Weight)
    loopN buf Z     r file = pure $ maybeToEither "impossible" r
    loopN buf (S n) r file = do
      putStrLn "\{show r}: (\{show n} tries remaining...)"
      _ <- readBufferData file buf 0 6
      d <- bufferData' buf
      case decode d of
        Ok w => loopN buf n     (Just w) file
        _    => loopN buf (S n) r        file

    onError : FileError -> IO String
    onError err = pure $ show err

||| Continuously read from the scale into a channel.
partial
run : (USBScale.Result -> IO Builtin.Unit) -> String -> IO Builtin.Unit
run post path = do
  Just buf <- newBuffer 6
    | Nothing => debug (the String "error, could not allocate buffer")
  Right _ <- withFile path Read onError (loop post buf)
    | Left err => debug err
  pure ()
  where
    loop
      : (Result -> IO Builtin.Unit)
      -> Buffer
      -> File
      -> IO (Either String Builtin.Unit)
    loop post buf file = do
      _ <- readBufferData file buf 0 6
      d <- bufferData' buf
      post (decode d)
      loop post buf file

    onError : FileError -> IO String
    onError err = pure $ show err


||| Spawn the scale reading thread
|||
||| The function parameter should post the USBScale.result to the main
||| event queue.
export partial
spawn : String -> (Result -> IO Builtin.Unit) -> IO ThreadID
spawn path post = fork (run post path)


||| XXX: Copy-Pasta from TUI/MainLoop.idr. Refactor to eliminate this duplication.
|||
||| See in-line comments for details.
namespace RefactorMe
  %hide Measures.Unit

  ||| Sum type which covers all relevant event sources.
  data Event
     = Stdin Char
     | Scale Result
     | Idle

  covering
  tick : Fifo Event -> IO ()
  tick chan = do
    usleep 250000
    put chan Idle
    tick chan

  ||| The main thing that is different is that this code has to handle
  ||| events from either the keyboard or the scale. There is no
  ||| non-blocking form of either API, so we must use threads and
  ||| channels to interleave these event streams.
  |||
  ||| Consequently, it takes two handlers. I need to think of a better
  ||| abstraction over event sources.
  |||
  ||| XXX: contribute some non-blocking variants of channelGet and
  ||| getChar to the standard library.
  covering
  runRaw
    :  (device : String)
    -> (onChar  : Char   -> stateT -> IO (Maybe stateT))
    -> (onScale : Result -> stateT -> IO (Maybe stateT))
    -> (render  : stateT -> IO Builtin.Unit)
    -> (init    : stateT)
    -> IO stateT
  runRaw device onChar onScale render init = do
    -- XXX: early on, I seemed to have better luck with
    -- `enableRawMode`, but this might be superstitious. investigate.
    _ <- enableRawMode
    altScreen True
    hideCursor
    saveCursor
    chan <- makeFifo
    _  <- USBScale.spawn device (put chan . Scale)
    -- XXX: superstitiously, I want to start reading from stdin only
    -- *after* it has been placed into raw mode. the thread may or may
    -- not inherit rawMode. investigate.
    _ <- fork $ readStdin    (put chan . Stdin)

    ret <- loop 0 chan init
    cleanup
    pure ret
  where
    ||| Read stdin one char at a time, post to the event queue.
    |||
    ||| Refactoring opportunity: do the escape sequence decoding on
    ||| this thread.
    covering
    readStdin : (Char -> IO Builtin.Unit) -> IO Builtin.Unit
    readStdin post = do
      char <- getChar
      post char
      readStdin post

    ||| restore terminal state as best we can
    cleanup : IO Builtin.Unit
    cleanup = do
      clearScreen
      restoreCursor
      showCursor
      altScreen False
      resetRawMode

    ||| The actual main loop
    |||
    ||| XXX: `i` is the loop count, which I'm using for debugging at
    ||| the moment.
    loop: Int -> Fifo Event -> stateT -> IO stateT
    loop i chan state = do
      beginSyncUpdate
      clearScreen
      moveTo origin
      render state
      endSyncUpdate

      next <- case !(get' chan) of
        Just (Scale nextScale) => onScale nextScale state
        Just (Stdin char)      => onChar  char      state
        Just Idle              => pure $ Just state
        Nothing                => pure $ Just state

      case next of
        Nothing => pure state
        Just next => loop (i + 1) chan next

  ||| Run a raw TUI application, decoding input escape sequences.
  |||
  ||| Use this function if you want escape sequence decoding, but do not
  ||| want to use the view abstraction for rendering screen contents.
  covering export
  runTUI
    :  (device  : String)
    -> (onKey   : Key    -> stateT -> IO (Maybe stateT))
    -> (onScale : Result -> stateT -> IO (Maybe stateT))
    -> (render  : stateT -> IO Builtin.Unit)
    -> (init    : stateT)
    -> IO stateT
  runTUI device onKey onScale render init = do
    ret <- runRaw
      device
      (interpretEsc onKey)
      (mapEsc onScale)
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
    => (device   : String)
    -> (onAction : actionT -> stateT -> IO stateT)
    -> (onScale  : Result  -> stateT -> IO (Maybe stateT))
    -> (init : stateT)
    -> IO stateT
  runView device onAction onScale init = do
    runTUI device wrapView onScale (paint Focused !(screen)) init
  where
    wrapView : Key -> stateT -> IO (Maybe stateT)
    wrapView k s = case handle k s of
      Update s    => pure $ Just s
      FocusParent => pure Nothing
      FocusNext   => pure $ Just s
      Run action  => pure $ Just $ !(onAction action s)


record SmartScale where
  constructor MkSmartScale
  containers : List (Barcode, Weight)
  scale      : Result
  cur        : Nat
  input      : Maybe $ SnocList Char
  frameNo    : Int


data Action = Tear | Reset


handleAlpha : Char -> SmartScale -> SmartScale
handleAlpha '*' self = { input := Just [<] } self
handleAlpha '#' self = case self.input of
  Just input => selectOrAdd input self
  Nothing => { input := Nothing } self
  where
    matchBarcode : Barcode -> (Barcode, Weight) -> Bool
    matchBarcode needle (barcode, _) = needle == barcode

    selectOrAdd : SnocList Char -> SmartScale -> SmartScale
    selectOrAdd input self = case fromDigits $ kcap input of
      Just bc => case findIndex (matchBarcode bc) self.containers of
        Just i  => { cur := finToNat i } self
        Nothing => { cur := 0, containers $= ((bc, 0.g) ::)} self
      Nothing => self
handleAlpha c self = case self.input of
  Just i  => case isDigit c of
    True => { input := Just $ i :< c } self
    False => self
  Nothing => self

incFrame : SmartScale -> SmartScale
incFrame self = { frameNo $= (+ 1) } self

View SmartScale Action where
  size self = MkArea 23 (length self.containers + 4)
  paint state window self = do
    let (top, bottom) = vsplit window 3
    withState Normal $ showTextAt top.nw $ "Gross Wt: \{show self.scale}"
    withState Normal $ case self.input of
      Just input => showTextAt (top.nw + MkArea 0 1) $ "Barcode: " ++ kcap input
      Nothing    => pure ()
    withState Normal $ showTextAt (top.nw + MkArea 0 2) $ show self.frameNo
    hline top.sw top.size.width
    loop 0 (shrink bottom) self.containers
    where
      focusedState : State -> Nat -> State
      focusedState Focused i = if (i == self.cur) then Focused else Normal
      focusedState state   _ = state

      loop : Nat -> Rect -> List (Barcode, Weight) -> IO ()
      loop _ _      []        = pure ()
      loop i window (x :: xs) = do
        let (top, bottom) = vsplit window 1
        let (left, right) = hsplit top 24
        let state = focusedState state i
        withState state $ showTextAt left.nw $ show (fst x)
        paint state right (snd x)
        loop (S i) bottom xs

  handle (Alpha c) self = Update $ incFrame $ handleAlpha c self
  handle Left self      = Update $ incFrame self
  handle Right self     = Update $ incFrame self
  handle Up self        = Update $ incFrame $ { cur $= pred } self
  handle Down self      = Update $ incFrame $ { cur $= S } self
  handle Delete self    = Run Reset
  handle Enter self     = Run Tear
  handle Tab self       = Update $ incFrame $ { cur $= S } self
  handle Escape self    = FocusParent

onAction : Action -> SmartScale -> IO SmartScale
onAction Tear self = case self.scale of
  Ok weight => pure $ tearCurrent weight self
  _         => pure $ self
  where
    tearCurrent : Weight -> SmartScale -> SmartScale
    tearCurrent w self = incFrame $ { containers $= loop self.cur } self
      where
        loop : Nat -> List (Barcode, Weight) -> List (Barcode, Weight)
        loop _ [] = []
        loop Z ((bc, _) :: xs) = (bc, w) :: xs
        loop (S n) (x :: xs) = x :: loop n xs
onAction Reset self = pure $ resetCurrent self
  where
    resetCurrent : SmartScale -> SmartScale
    resetCurrent self = incFrame $ { containers $= loop self.cur } self
      where
        loop : Nat -> List (Barcode, Weight) -> List (Barcode, Weight)
        loop _ [] = []
        loop Z ((bc, _) :: xs) = (bc, 0.g) :: xs
        loop (S n) (x :: xs) = x :: loop n xs

onScale : Result -> SmartScale -> IO (Maybe SmartScale)
onScale result self = do
  pure $ Just $ incFrame $ { scale := result } self

||| Entry point for basic scale command.
export partial
main : List String -> IO Builtin.Unit
main ("--once" :: path :: _) = do
  weight <- getWeight path
  putStrLn $ show weight
main (device :: _) = do
  _ <- runView device onAction onScale (MkSmartScale [] Empty 0 Nothing 0)
  pure ()
main _ = do
  debug "No device file given"
  pure ()
