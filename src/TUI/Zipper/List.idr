module TUI.Zipper.List


import Data.List1
import Data.SnocList
import Util


%default total


||| A Zipper over a list of values
public export
record Zipper a where
  constructor Z
  ||| Characters left of the cursor.
  |||
  ||| The tail of this list is the insertion point.
  left   : SnocList a
  ||| Characters right of the cursor.
  right  : List a

||| Get the item under the cursor, if there is one.
public export
cursor : Zipper a -> Maybe a
cursor (Z [<]      l) = Nothing
cursor (Z (_ :< x) l) = Just x

||| Decompose a Zipper into Left, Cursor, Right
|||
||| A useful helper routine for client code wishing to traverse the
||| zipper.
public export
decompose : Zipper a -> (List a, Maybe a, List a)
decompose (Z [<]       r) = ([],        Nothing, r)
decompose (Z (xs :< x) r) = (toList xs, Just x,  r)

||| Create an empty zipper
public export
empty : Zipper a
empty = Z [<] []

||| Move the cursor to the beginning of the zipper.
public export
rewind : Zipper a -> Zipper a
rewind (Z [<] r) = Z [<] r
rewind (Z l   r) = Z [<] $ (toList l) ++ r

||| Create a zipper from a list
public export
fromList : List a -> Zipper a
fromList list = Z (cast list) []

||| Convert a zipper to a list.
public export
toList : Zipper a -> List a
toList self = (toList self.left) ++ (toList self.right)

||| Convert a zipper to a non-empty list or null.
public export
toList1 : Zipper a -> Maybe (List1 a)
toList1 = fromList . toList

||| Insert an element at the current position.
public export
insert : a -> Zipper a -> Zipper a
insert c = { left $= (:< c) }

||| Delete a character.
export
delete : Zipper a -> Zipper a
delete = { left $= liat }

||| Move insertion point rightward
public export
goRight : Zipper a -> Zipper a
goRight (Z l [])        = Z l        []
goRight (Z l (x :: xs)) = Z (l :< x) xs

||| Move insertion point rightward
public export
goLeft : Zipper a -> Zipper a
goLeft (Z [<] r)       = Z [<] r
goLeft (Z (xs :< x) r) = Z xs    (x :: r)

||| Get the length of both halves of the zipper
public export
length : Zipper a -> Nat
length self = length self.left + length self.right

||| Replace element at the current position by the application of `f`.
|||
||| If the zipper is empty, returns an empty zipper.
|||
||| If the zipper has been rewound, has no effect.
public export
update : (a -> a) -> Zipper a -> Zipper a
update _ self@(Z [<] [])        = self
update f self@(Z [<] (x :: xs)) = self
update f self@(Z (xs :< x) r)   = Z (xs :< f x) r

||| Replace element at the current position.
|||
||| If the zipper is empty, returns an empty zipper.
|||
||| If the zipper has been rewound, has no effect.
public export
replace : a -> Zipper a -> Zipper a
replace x = update (const x)

||| Set the cursor over the Nth child from the left.
|||
||| Stops at the end if `n` is greater than the length of the zipper.
public export
seekTo : Nat -> Zipper a -> Zipper a
seekTo n self = loop n $ rewind self
  where
    loop : Nat -> Zipper a -> Zipper a
    loop Z     self = self
    loop (S n) self = seekTo n (goRight self)

||| Advance rightward until `p x` gives `True`.
|||
||| If found, returns a zipper with the cursor at the given
||| position. If we reach the end of the zipper, returns Nothing.
public export
seekRight : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
seekRight p self@(Z [<]       []) = Nothing
seekRight p self@(Z (xs :< x) []) = case p x of
  True => Just self
  False => Nothing
seekRight p self@(Z l  (x :: xs))  = case p x of
  True  => Just $ goRight self
  False => seekRight p $ assert_smaller self $ goRight self

||| Advance right from the beginning, until p returns True.
|||
||| If found, return a zipper with the cursor at the given
||| position. If we reach the end, returns Nothing.
public export
find : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
find p self = seekRight p (rewind self)

||| Find an element satisfying some predicate, or insert a default element.
|||
||| Always succeeds. The zipper will either be advanced to the first
||| element satisfying the predicate, or the cursor will be under
||| the newly-inserted default element.
public export
findOrInsert : (a -> Bool) -> Lazy a -> Zipper a -> Zipper a
findOrInsert p x self = case find p self of
  Just self => self
  Nothing   => insert (force x) self
