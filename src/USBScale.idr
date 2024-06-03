||| Driver for talking to 510 Scale over USB.
|||
||| The scale sends a 6-byte packet, which must be manually swizzled
||| to decode the weight.
module USBScale


import Barcode
import Container
import Control.ANSI
import Data.Bits
import Data.Buffer
import Data.Either
import Data.Vect
import Derive.Prelude
import JSON.Derive
import Measures
import System.Concurrency
import System
import System.File
import TUI
import Util
import Zipper


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
%runElab derive "Result" [Show,Ord,Eq,FromJSON,ToJSON]

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

namespace Container
  %hide Measures.Unit

  View Container where
    -- size here is just a guess, but it should be a fixed grid
    -- up to 13 chars for the barcode, plus padding
    -- 10 digits each for gross, tear, and net
    size _ = MkArea (14 + 10 + 10 + 10) 1

    paint state window self = do
      let (top,     bottom) = vsplit window 1
      let (barcode, top   ) = hsplit top 13
      let (tear,    top   ) = hsplit top 10
      let (gross,   net   ) = hsplit top 10
      paint state barcode self.id
      paint state tear    self.tear
      paint state gross   self.gross
      paint state net     self.net

namespace SmartScale
  ||| An MVP of my "Smart Scale" concept
  |||
  ||| Basically: we have a list of containers and a digital scale.
  |||
  ||| We can tear each container independently.
  |||
  ||| We can store initial and final weights, so at a glance you can
  ||| see the how much you have added or removed from each container.
  |||
  ||| Explanation of User Interaction
  |||
  ||| - We start off in default mode, with no containers and no selection.
  ||| - The user adds a container by entering its barcode
  ||| - The barcode scanner works like a keyboard, so we can't
  |||   distinguish it from key presses.
  |||   - It's configured to prefix the barcode with a `*` symbol.
  |||   - `*` always transfers focus to the character list.
  |||     - subsequent digits are appended to the list
  |||     - enter tries to parse the digits as a barcode, if it succeeds it is focused
  |||       - if it's not in the list, we add a new container to the list
  ||| - if we have a selected container
  |||   - enter will store the current weight as the gross weight of the container
  |||   - delete will store the current weight as the tear weight of the container
  |||   - escape will reset both weights
  record SmartScale where
    constructor MkSmartScale
    containers : Zipper Container
    scale      : Result
    barcode    : Editor String
    image      : String

  ||| These are the global actions for this component.
  data Action
    = Prev
    | Next
    | Tear
    | Store
    | Reset
    | Barcode (Editor.Action String TextInput.Action)
    | SetScale Result
    | SetImage String

  ||| Update the selected container by the application of `f`
  updateSelected : (Container -> Container) -> SmartScale -> SmartScale
  updateSelected f = { containers $= update f }

  ||| Check whether user has entered a valid barcode
  validateBarcode : SmartScale -> Maybe Barcode
  validateBarcode self = fromDigits $ toString !self.barcode

  ||| Select or add the current barcode to the container list
  select : SmartScale -> SmartScale
  select self = case validateBarcode self of
    Nothing => self
    Just bc@(User id) => {
      containers $= findOrInsert (hasBarcode bc) (empty id Reusable),
      barcode := Nothing
    } self
    Just food => self

  ||| Apply `f` to `x` if the given result is a weight
  |||
  ||| If the given Result isn't a valid weight, returns the original
  ||| value.
  public export
  withCurrentWeight
    :  (Weight -> Container -> Container)
    -> SmartScale
    -> SmartScale
  withCurrentWeight f self = case self.scale of
    Ok weight => updateSelected (f weight) self
    _         => self

{-
  ||| Handles events when the barcode input is not active
  |||
  ||| Receiving a '*' will give focus to the barcode input.
  handleDefault : Key -> SmartScale -> Response (Maybe ()) TUI.Action
  handleDefault (Alpha 'q') self = Yield Nothing
  handleDefault (Alpha 't') self = withWeight case self.scale of
    Do Tear
  handleDefault (Alpha 's') self = Do Store
  handleDefault (Alpha 'r') self = Do Reset
  handleDefault (Alpha _)   self = Ignore
  handleDefault Left        self = Ignore
  handleDefault Right       self = Ignore
  handleDefault Up          self = Do Next
  handleDefault Down        self = Do Prev
  handleDefault Delete      self = Do Tear
  handleDefault Enter       self = Do Store
  handleDefault Tab         self = Do Next
  handleDefault Escape      self = Yield Nothing
 -}

  ||| Handle nested updates to the barcode field.
  |||
  ||| XXX: Hopefully this points the way toward how to nest components.
  updateBarcode
    : TextInput.Action
    -> SmartScale
    -> Result SmartScale (List Container)
  updateBarcode action self = case Component.update (Do action) self.barcode of
    xxx => ?hole

  ||| Implement Component for our SmartScale
  Component SmartScale (List Container) SmartScale.Action where
    update Prev              self = Left $ { containers $= goLeft }  self
    update Next              self = Left $ { containers $= goRight } self
    update Tear              self = Left $ withCurrentWeight setTear  self
    update Store             self = Left $ withCurrentWeight setGross self
    update Reset             self = Left $ updateSelected reset self
    update BarcodeBegin      self = Left $ { barcode := empty } self
    update (BarcodeKey a)    self = updateBarcode a self
    update (SetScale result) self = Left $ { scale   := result } self
    update (SetImage sixel)  self = Left $ { image   := sixel }  self

    size self = MkArea 23 (length self.containers + 4)
    paint state window self = do
      let (top, bottom) = vdivide window 3
      hpane
        Normal
        top
        self.image
        (Util.Data.Either.fromMaybe
          "Scan or Type '*' to enter barcode"
          self.barcode)
        23
      -- xxx: document sixel stuff
      hline top.sw top.size.width
      case self.containers.left of
        Lin => do
          paint @{string} Focused bottom "Barcode      Tear      Gross     Net "
        xs => do
          bottom <- packTop @{string} Normal bottom "Barcode      Tear      Gross     Net "
          ignore $ paint @{vertical} state bottom self.containers

    -- '*' always sets the barcode input to an empty buffer
    -- this way, if the barcode scanner is used on a partial buffer,
    -- we clear it before accepting chars from the scanner.
    handle (Alpha '*') self = Update $ { barcode := Just (empty Select) } self
    handle key self = case self.barcode of
      -- if the barcode view is present, it get focus
      Just barcode => case handle key barcode of
        (Update x)  => Update $ { barcode := Just x } self
        Exit        => Update $ { barcode := Nothing } self
        (Do x)      => Do x
      -- if not, we handle them here.
      Nothing      => handleDefault key self

{-

  ||| Update the current scale value when we receive a new packet.
  export
  onScale : List Bits8 -> SmartScale -> IO SmartScale
  onScale result self = do
    pure $ Left $ {scale := decode result} self

  ||| Render the given image path in sixel format when we receive an image event.
  |||
  ||| One issue here is that we have don't have access to the window
  ||| here, so we have to choose a fixed image size to render to. But
  ||| apparently it's important not call out to a subprocess while
  ||| rendering.
  onImage : String -> SmartScale -> IO SmartScale
  onImage path self = do
    (sixel, code) <- assert_total $ run ["chafa", "-s", "20", "upload/decoded.0"]
    pure $ Left $ { image := sixel } self

||| Entry point for basic scale command.
export partial
main : List String -> IO Builtin.Unit
main ("--once" :: path :: _) = do
  weight <- getWeight path
  putStrLn $ show weight
main _ = pure ()

main _ = do
  _ <- runComponent
    [
     On "Scale" onScale,
     On "Image" onImage
    ]
    (MkSmartScale empty Empty Nothing "No image")
  pure ()
