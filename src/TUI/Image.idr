module TUI.Image


import System
import System.File
import TUI.Geometry
import TUI.Painting
import TUI.View


%default total


||| The bit depths supported by Chafa
public export
data BitDepth = B2 | B8 | B16 | B240 | B256 | Full | Max

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
putImage : Pos -> Image -> Context ()
putImage pos self = do
  moveTo pos
  case self.contents of
    Nothing => do
      cheat $ fflush stdout
      cheat $ putStr self.altText
      cheat $ fflush stdout
    Just contents => cheat $ putStr contents

||| Render a sixel image from the given file name.
|||
||| The image will be scaled to fit the given screen rectangle.
|||
||| XXX: This is a stopgap which requires the `chafa` binary. I'm
||| not sure how safe this is, use at your own risk. There aren't any
||| idris image decoding libraries, native or otherwise.
export covering
sixelFromPath : BitDepth -> String -> String -> Area -> IO Image
sixelFromPath depth path alt size = do
  case !(run cmdline) of
    (sixel, 0) => pure $ MkImage size (Just sixel) alt
    _          => pure $ MkImage size Nothing alt
where
  ||| Convert bit-depth to a command line argument
  bitDepth : BitDepth -> List String
  bitDepth B2   = ["-c", "2"   ]
  bitDepth B8   = ["-c", "8"   ]
  bitDepth B16  = ["-c", "16"  ]
  bitDepth B240 = ["-c", "240" ]
  bitDepth B256 = ["-c", "256" ]
  bitDepth Full = ["-c", "full"]
  bitDepth Max  = []

  sizeArgs : Area -> List String
  sizeArgs (MkArea width height) = ["-s", "\{show width}x\{show height}"]

  cmdline : List String
  cmdline = ["chafa"] ++ (sizeArgs size) ++ (bitDepth depth) ++ [path]


||| An image can also be used directly as a view.
export
View Image where
  size = (.size)
  paint _ window self = putImage window.nw self
