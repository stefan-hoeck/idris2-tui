module Image


import System
import TUI.Geometery
import TUI.Painting
import TUI.View


%default total


||| An abstracct image.
|||
||| calling `putImage` will draw it to the screen at the current
||| cursor position.
export
record Image where
  constructor MkImage
  size     : Area
  contents : String

||| Put the given image to the screen at the cursor position.
|||
||| There is no way to control the clipping of this image.
export
putImage : Pos -> Image -> IO ()
putImage pos image = showTextAt pos image.contents

||| Render a sixel image from the given file name.
|||
||| The image will be scaled to fit the given screen rectangle.
|||
||| XXX: This is a stopgap which requires the `chafa` binary. I'm
||| not sure how safe this is, use at your own risk. There aren't any
||| idris image decoding libraries, native or otherwise.
partial export
sixelFromPath : String -> Area -> IO (Maybe Image)
sixelFromPath path size = do
  case !(run [
    "chafa",
    "-s", "\{show size.width}x\{show size.height}",
    path
  ]) of
    (sixel, 0) => pure $ Just $ MkImage size sixel
    _          => pure Nothing

||| An image can also be used directly as a view.
export
View Image where
  size = (.size)
  paint _ window self = putImage window.nw self
