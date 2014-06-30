-- file: Main.hs
-- image processing and converting functions

-- import all stuff
module Image where


import Math

import Data.Array.Unboxed as U
import Data.Array.IArray  as IA
import Codec.Image.DevIL as DEVIL
import Control.DeepSeq


type Resolution = (Int, Int)

-- TODO: use unboxed types??
type Color3f = (Double, Double, Double) -- RGB float format (0..1 for each channel)
type Color3i = (Int, Int, Int)          -- RBG integer format (0..255 for each channel)

data Image2Df = Image2Df Resolution [Color3f]

instance NFData Image2Df where
  rnf (Image2Df res pixels) = res `deepseq` pixels `deepseq` ()

data Image2Di = Image2Di Resolution [Color3i]

instance NFData Image2Di where
  rnf (Image2Di res pixels) = res `deepseq` pixels `deepseq` ()

type DevILImage = U.UArray (Int,Int,Int) Word8




-- image2d to Unboxed

-- convert without checking of resolution
convertImageUnsafe :: Image2Df -> Image2Di
convertImageUnsafe (Image2Df res pixels) = Image2Di res (map convertPixel pixels)
    where convertPixel = mapTuple3 (toInt . clamp)
          toInt f      = round (f * 255.0)

-- TODO: convert image in the safe way!


-- TODO: using listArray instead of array contructor is easier??
convertToDevILFormat :: Image2Di -> DevILImage
convertToDevILFormat (Image2Di res pixels) = IA.array ((0,0,0),(h-1,w-1,3)) packed
              
    where (h,w)   = res
          indexes = [(x,y,alpha) | y     <- [0..h-1],
                                   x     <- [0..w-1],
                                   alpha <- [0..3] :: [Int]]

          flattenImg ((r,g,b):xs) = r:g:b:defaultAlpha:(flattenImg xs)
          flattenImg []           = []
          defaultAlpha            = 255    
                            
          packed = zip indexes $ map fromIntegral $ flattenImg pixels
