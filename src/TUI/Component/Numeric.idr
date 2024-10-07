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

||| A component for numeric input:
|||
||| Ignores non-numeric keypresses, and cannot hold a non-numeric
||| value.
|||
||| Other features include:
||| - press `-` at any time to swap sign.
||| - dot ignored after first press.
||| - backspace clears whole field.
||| - TBD: increment / decrement actions.
export
record Numeric a where
  constructor N
  digits   : Digits
  sign     : Bool

||| Convert a character to a digit for event handling.
charToDigit : Char -> Maybe (Fin 10)
charToDigit char = integerToFin (cast $ ord char - ord '0') 10

||| Convert a digit to a character.
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
paintNumeric : Char -> State -> Rect -> Numeric a -> Context ()
paintNumeric symbol state window self = do
  showCharAt window.nw symbol
  case state of
      Focused => reverseVideo
      _       => pure ()
  showTextAt (window.nw + MkArea 2 0) (toString self)
  sgr [Reset]

||| Interface for supported numeric types.
interface Supported a where
  (.value)    : Numeric a -> Maybe a
  symbol      : Char
  charToInput : Char -> Maybe Input

Supported Nat where
  (.value)       = parsePositive . toString
  symbol         = cast 0x2115
  charToInput c  = Digit <$> charToDigit c

Supported Integer where
  (.value)        = parseInteger . toString
  symbol          = cast 0x2124
  charToInput '-' = Just Minus
  charToInput c   = Digit <$> charToDigit c

Supported Double where
  (.value)        = parseDouble . toString
  symbol          = cast 0x211d
  charToInput '-' = Just Minus
  charToInput '.' = Just Dot
  charToInput c   = Digit <$> charToDigit c

||| Handle a supported keypress.
handleChar : Supported a => Char -> Numeric a -> Response (Numeric a) a
handleChar char self = case charToInput {a = a} char of
  Nothing => Ignore
  Just i  => Do $ insert i self

||| Implement Model for supported number types.
export
Supported a => View (Numeric a) where
  size self = MkArea (width self.digits) 1
  paint state window self = paintNumeric (symbol {a = a}) state window self

||| Implement Model for supported number types.
export
handle : Supported a => Handler (Numeric a) a
handle (Alpha char) self = handleChar char self
handle Delete       self = Do $ clear self
handle Left         self = Yield Nothing
handle Enter        self = Yield self.value
handle Escape       self = Yield Nothing
handle _            self = Ignore

||| Create a numeric widget from a number value.
export
numeric : a -> Numeric a
numeric value = N {
  digits   = empty, -- xxx: decode
  sign     = False
}
