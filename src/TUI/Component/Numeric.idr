||| Minimalist terminal UI framework.
|||
||| A Numeric Input Widget
module TUI.Component.Numeric


import Data.Fin
import Data.String
import Data.SnocList
import TUI.Component
import Util


%default total


||| The set of symbols we accept.
export
data Input
  = Digit (Fin 10)
  | Dot
  | Minus

||| A sequence of digits representing a valid positive number.
data Digits
  = Integral (SnocList (Fin 10))
  | Decimal  (SnocList (Fin 10)) (SnocList (Fin 10))

charToDigit : Char -> Maybe (Fin 10)
charToDigit char = integerToFin (cast $ ord char - ord '0') 10

digitToChar : Fin 10 -> Char
digitToChar d = cast $ (ord '0') + cast (finToNat d)

||| Insert a single digit value.
insertDigits : Fin 10 -> Digits -> Digits
insertDigits d (Integral ds)  = Integral (ds :< d)
insertDigits d (Decimal i ds) = Decimal  i (ds :< d)

||| An empty digits value.
export empty : Digits ; empty = Integral [<]

||| Convert a digit string to a string
digitsToString : Digits -> String
digitsToString self = case self of
  Integral   ds => impl ds
  Decimal  i ds => "\{show i}.\{impl ds}"
  where
    impl : SnocList (Fin 10) -> String
    impl [<] = "0"
    impl xs  = kcap $ digitToChar <$> xs

||| An editable number.
|||
||| This filters out non-numeric keypresses, and cannot hold a
||| non-numeric value.
|||
||| Other features include:
||| - press `-` at any time to swap sign
||| - dot ignored after first press.
||| - backspace clears whole input (as it's usually easier to start again).
export
record Numeric a where
  constructor N
  digits   : Digits
  sign     : Bool
  step     : a

||| Get the width of the entire control, including symbols and padding.
export
width : Digits -> Nat
width (Integral  ds)    = 3 + length ds
width (Decimal   is ds) = max 6 (3 + length is + 1 + length ds)

||| Insert a single digit.
export
insert : Input -> Numeric a -> Numeric a
insert (Digit d) self = { digits $= insertDigits d } self
insert Dot       self = case self.digits of
  Integral ds   => { digits := Decimal ds [<] } self
  Decimal  _  _ => self
insert Minus     self = { sign $= not } self

||| Clear the input field.
export
clear : Numeric a -> Numeric a
clear = { digits := empty }

||| Toggle the sign of the input
export
negate : Numeric a -> Numeric a
negate = { sign $= not }

||| Get a string representation of the component.
export
toString : Numeric a -> String
toString self = if self.sign
  then "-\{digitsToString self.digits}"
  else " \{digitsToString self.digits}"

||| Generic paint function for all variants of numeric widget.
paintNumeric : Char -> State -> Rect -> Numeric a -> IO ()
paintNumeric symbol state window self = do
  showCharAt window.nw symbol
  case state of
      Focused => reverseVideo
      _       => pure ()
  showTextAt (window.nw + MkArea 2 0) (toString self)
  sgr [Reset]

||| Model implementation for Numeric
namespace Model

  public export
  interface ToValue a where
    (.value) : Numeric a -> Maybe a

  export ToValue Nat     where (.value) = parsePositive . toString
  export ToValue Integer where (.value) = parseInteger  . toString
  export ToValue Double  where (.value) = parseDouble   . toString

  public export
  interface FromValue a where
    from : a -> Numeric a

  public export
  data Action
    = Clear
    | Negate
    | Insert Input

  export
  implementation
       ToValue a
    => Model (Numeric a) (Maybe a) Model.Action
  where
    update Clear  self     = Left  $ clear  self
    update Negate self     = Left  $ negate self
    update (Insert i) self = Left  $ insert i self

namespace View

  public export
  interface Symbol a where
    symbol : Char

  -- the unicode symbols which decorate the widget
  export Symbol Nat     where symbol = cast 0x2115
  export Symbol Integer where symbol = cast 0x2124
  export Symbol Double  where symbol = cast 0x211d

  ||| View is implemented for any number type which defines a symbol.
  export
  Symbol a => View (Numeric a) where
    size self = MkArea (width self.digits) 1
    paint state window self = paintNumeric (symbol {a = a}) state window self

namespace Controller

  ||| Event handling common to all variants.
  handleCommon
    :  ToValue a
    => Key
    -> (Char -> Maybe Input)
    -> Numeric a
    -> Response (Maybe a) Model.Action
  handleCommon (Alpha char) f self = fromMaybe Ignore $ (Do . Insert) <$> f char
  handleCommon Delete       _ self = Do Clear
  handleCommon Left         _ self = Yield Nothing
  handleCommon Enter        _ self = Yield self.value
  handleCommon Escape       _ self = Yield Nothing
  handleCommon _            _ self = Ignore

  ||| This controller doesn't accept decimal or negation
  Controller (Numeric Nat) (Response (Maybe Nat) Model.Action) where
    handle key self = handleCommon key ((map Digit) . charToDigit) self

  ||| This implementation ignores decimals, but handles the minus sign.
  export
  Controller (Numeric Integer) (Response (Maybe Integer) Model.Action) where
    handle key self = handleCommon key special self
      where
        special : Char -> Maybe Input
        special '-' = Just Minus
        special c   = Digit <$> charToDigit c

  ||| This implementation handles both decimal and minus sign.
  export
  Controller (Numeric Double) (Response (Maybe Double) Model.Action) where
    handle key self = handleCommon key special self
      where
        special : Char -> Maybe Input
        special '-' = Just Minus
        special '.' = Just Dot
        special c   = Digit <$> charToDigit c

||| Create a numeric widget from a number value.
export
numeric : a -> a -> actionT -> Numeric a
numeric value step onChange = N {
  digits   = empty, -- xxx: decode
  sign     = False,
  step     = step
}
