import Data.List as L
import Data.Array.Unboxed as U
import Data.Array.IArray as I

import Control.Monad

import System.Directory

import Codec.Image.DevIL as DEVIL



-- definition of the simple 3D-vector
data Vec3D a = Vec3D a a a deriving (Eq, Show)

(.+.) :: Num a => Vec3D a -> Vec3D a -> Vec3D a
(.+.) (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D (x1+x2) (y1+y2) (z1+z2)

(.*) :: Num a => a -> Vec3D a -> Vec3D a
(.*) scalar (Vec3D x y z) = Vec3D (scalar*x) (scalar*y) (scalar*z)

(*.) :: Num a => Vec3D a -> a -> Vec3D a
(*.) v scalar = scalar .* v

-- example: (4 .* (Vec3D 2 2 4) *. 2)

dot :: Num a => Vec3D a -> Vec3D a -> a
dot (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = (x1*x2)+(y1*y2)+(z1*z2)

len :: Floating a => Vec3D a -> a
len v = sqrt (v `dot` v)

normalize :: Floating a => Vec3D a -> Vec3D a
normalize v = v *. (1.0 / len v)

-- TODO: discuss, why we couldn't just make our vector instance of Num

data Ray = Ray {  origin    :: Vec3D Double,
                  direction :: Vec3D Double  } deriving (Eq, Show)


-- global variables
width  = 400 :: Int
height = 400 :: Int

-- render :: resolution -> dist form origin -> [pixel]

-- image format
-- [(r,g,b,a)] list => convert to UArray

type Resolution = (Int, Int)

-- TODO: use unboxed types??
type Color3f = (Float, Float, Float) -- RGB float format (0..1 for each channel)
type Color3d = (Int, Int, Int) -- RBG integer format (0..255 for each channel)

data Image2Df = Image2Df Resolution [Color3f]
data Image2Dd = Image2Dd Resolution [Color3d]

type DevILImage = U.UArray (Int,Int,Int) Word8

-- image2d to Unboxed

-- covert without checking of resolution
convertImageUnsafe :: Image2Df -> Image2Dd
convertImageUnsafe (Image2Df res pixels) = Image2Dd res (map convertPixel pixels)
    where convertPixel = mapTuple (toInt . cutRange)
          cutRange f 
                | f <= 0.0 = 0.0
                | f >= 1.0 = 1.0
                | otherwise = f
          toInt f = round (f * 255.0)


mapTuple :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple  f (x,y,z)= (f x, f y, f z)


testImage :: Image2Df
testImage = Image2Df (width, height) 
                     [mapTuple pixFunc (x,y,200) | y <- [0..height-1], 
                                                   x <- [0..width-1]]
    where pixFunc p = (fromIntegral p :: Float) / 400.0
    -- TODO: dont't use literal 400.0 !! unsafe
    -- better make a choice betwee 2 values

testDevImg1 = convertToDevILFormat . convertImageUnsafe $ testImage


convertToDevILFormat :: Image2Dd -> DevILImage
convertToDevILFormat (Image2Dd res pixels) = I.array ((0,0,0),(h-1,w-1,3)) packed
              
    where (h,w)   = res
          indexes = [(x,y,alpha) | y     <- [0..h-1],
                                   x     <- [0..w-1],
                                   alpha <- [0..3] :: [Int]]

          flattenImg ((r,g,b):xs) = r:g:b:alpha:(flattenImg xs)
          flattenImg []           = []
          alpha                   = 255    
                              
          packed = zip indexes $ map fromIntegral $ flattenImg pixels
                                    



                                                                          



main :: IO ()
--main = print 

main = do
         ilInit -- initialize DevIL library
         let outFile = "test.png"
         exist <- doesFileExist outFile
         when exist $ removeFile outFile
         DEVIL.writeImage outFile testDevImg1


