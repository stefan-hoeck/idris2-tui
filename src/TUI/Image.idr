module TUI.Image


import System
import System.File
import TUI.Geometry
import TUI.Painting
import TUI.View


%default total


||| An abstract image.
|||
||| calling `putImage` will draw it to the screen at the current
||| cursor position.
export
record Image where
  constructor MkImage
  size     : Area
  contents : Maybe String
  altText  : String

export
placeholder : String -> Area -> Image
placeholder alt size = MkImage size Nothing alt


||| Put the given image to the screen at the cursor position.
|||
||| There is no way to control the clipping of this image.
export
putImage : Pos -> Image -> IO ()
putImage pos self = do
  moveTo pos
  case self.contents of
    Nothing => do
      fflush stdout
      putStr self.altText
      fflush stdout
    Just contents => putStr contents

||| Render a sixel image from the given file name.
|||
||| The image will be scaled to fit the given screen rectangle.
|||
||| XXX: This is a stopgap which requires the `chafa` binary. I'm
||| not sure how safe this is, use at your own risk. There aren't any
||| idris image decoding libraries, native or otherwise.
export covering
sixelFromPath : String -> String -> Area -> IO Image
sixelFromPath path alt size = do
  case !(run [
    "chafa",
    "-s", "\{show size.width}x\{show size.height}",
    "-c", "8",
    path
  ]) of
    (sixel, 0) => pure $ MkImage size (Just sixel) alt
    _          => pure $ MkImage size Nothing alt

||| An image can also be used directly as a view.
export
View Image where
  size = (.size)
  paint _ window self = putImage window.nw self
