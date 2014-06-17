import Data.List as L
import Data.Array.Unboxed as U
import Data.Array.IArray as I
import Control.Monad
import System.Directory

import Codec.Image.DevIL as DEVIL



-- definition of the simple 3D-vector
data Vec3D a = Vec3D a a a deriving (Eq, Show)

(|+|) :: Num a => Vec3D a -> Vec3D a -> Vec3D a
(|+|) (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D (x1+x2) (y1+y2) (z1+z2)

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

-- not tested yet!!! need testing!
cross :: Floating a => Vec3D a -> Vec3D a -> Vec3D a
cross (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D x3 y3 z3
    where x3 = y1 * z2 - z1 * y2
          y3 = z1 * x2 - x1 * z2
          z3 = x1 * y2 - y1 * x2

mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3  f (x,y,z)= (f x, f y, f z)

-- TODO: discuss, why we couldn't just make our vector instance of Num

data Ray = Ray {  getRayOrigin    :: Vec3D Double,
                  getRayDirection :: Vec3D Double  } deriving (Eq, Show)

-- starting rays
-- for now we always start from (0,0,0)
-- we need to properly set the projection camera 
-- in the scene

-- (height!, width!)
type ScreenSize = (Double, Double) -- size of the scene camera's screen in relative units
type Distance = Double -- screen offset from the origin of axis

-- TODO: draw a proper image, describing the whole ray transformation pipeline

-- TODO (error checking) -> what if user defines resolution like (0,-1)

-- resolution told us, how many rays to create
castRays :: ScreenSize -> Distance -> Resolution -> Image2Df
castRays (h,w) dist res@(xRes,yRes) = Image2Df res pixels

    where rays = [toRay x y | x <- [0..xResF-1], 
                              y <- [0..yResF-1]]

          pixels = map (mapTuple3 (\_ -> 1.0) ) rays

          toRay px py = ((px / xResF - 0.5) * w, 
                         (py / yResF - 0.5) * h,
                         dist) -- z = distance

          (xResF, yResF) = (fromIntegral xRes, 
                            fromIntegral yRes)



-- intersection :: Ray -> [Objects] -> (PointOfIntersection, Normal)
-- intersectionToSphere :: Ray -> Sphere -> Maybe (normal, pointOfIntersection)
-- intersectionPoly .....
-- intersection....






-- scene objects
data SceneObject = Polygon (Vec3D Double) (Vec3D Double) (Vec3D Double) | 

                   Sphere { getSphereRaduis   :: Double,
                            getSpherePosition :: Vec3D Double}

                   deriving (Eq, Show)


-- TODO: should be composed with object???
-- TODO: make more generic, i.e. material = diffuse | reflective
data Material = Empty





-- render :: resolution -> dist form origin -> [pixel]

-- image format
-- [(r,g,b,a)] list => convert to UArray

type Resolution = (Int, Int)

-- TODO: use unboxed types??
type Color3f = (Float, Float, Float) -- RGB float format (0..1 for each channel)
type Color3i = (Int, Int, Int) -- RBG integer format (0..255 for each channel)

data Image2Df = Image2Df Resolution [Color3f]
data Image2Di = Image2Di Resolution [Color3i]

type DevILImage = U.UArray (Int,Int,Int) Word8



-- image2d to Unboxed

-- convert without checking of resolution
convertImageUnsafe :: Image2Df -> Image2Di
convertImageUnsafe (Image2Df res pixels) = Image2Di res (map convertPixel pixels)
    where convertPixel = mapTuple3 (toInt . cutRange)
          cutRange f 
                | f <= 0.0 = 0.0
                | f >= 1.0 = 1.0
                | otherwise = f
          toInt f = round (f * 255.0)

-- TODO: vonvet image safe!


-- TODO: using listArray instead of array contructor is easier??
convertToDevILFormat :: Image2Di -> DevILImage
convertToDevILFormat (Image2Di res pixels) = I.array ((0,0,0),(h-1,w-1,3)) packed
              
    where (h,w)   = res
          indexes = [(x,y,alpha) | y     <- [0..h-1],
                                   x     <- [0..w-1],
                                   alpha <- [0..3] :: [Int]]

          flattenImg ((r,g,b):xs) = r:g:b:alpha:(flattenImg xs)
          flattenImg []           = []
          alpha                   = 255    
                            
          packed = zip indexes $ map fromIntegral $ flattenImg pixels









-- global variables -------
width  = 400 :: Int
height = 400 :: Int

testImage :: Image2Df
testImage = Image2Df (width, height) 
                     [mapTuple3 pixFunc (x,y,200) | y <- [0..height-1], 
                                                    x <- [0..width-1]]
    where pixFunc p = (fromIntegral p :: Float) / 400.0
    -- TODO: dont't use literal 400.0 !! unsafe
    -- better make a choice betwee 2 values

testDevImg1 = convertToDevILFormat . convertImageUnsafe $ testImage
testImg2    = convertToDevILFormat . convertImageUnsafe $ 
                      castRays (2.0,2.0) 1.0 (height, width)

---------------------------



                                    



                                                                          



main :: IO ()
--main = print 

main = do
         ilInit -- initialize DevIL library
         let outFile = "test.png"
         exist <- doesFileExist outFile
         when exist $ removeFile outFile
         DEVIL.writeImage outFile testImg2


