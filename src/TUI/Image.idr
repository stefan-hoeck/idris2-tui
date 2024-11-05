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

||| Rudimentary support for Sixel images
|||
||| Actually, it just shells out to `chafa`, which is not great. The
||| trouble is that we need an image decoder library before we can do
||| sixel encoding in Idris, and that's a lot of FFI work.
|||
||| For now, explore the possibilities with this ugly hack.
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
export
putImage : Pos -> Image -> Context ()
putImage pos self = do
  moveTo pos
  case self.contents of
    Nothing       => putStr self.altText
    Just contents => putStr contents

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
