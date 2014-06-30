-- Main.hs

module Main 
(
  main
) 
where

import Math
import Image
import Scene

import Data.Function
import Data.Maybe
import Control.Monad
import Data.Time

import Data.List as L hiding (intersect)

import System.Directory
import Codec.Image.DevIL as DEVIL

import Prelude hiding ((+),(-),(*),(/), fromIntegral)
import qualified Prelude as P
import NumericPrelude.Numeric

import Control.Exception.Base (evaluate)
import Control.DeepSeq (force)


-- (height!, width!) <- right format
type ScreenSize = (Double, Double) -- size of the scene camera's screen in relative units
type Distance = Double -- screen offset from the origin of axis

data Camera = Camera ScreenSize Distance


--data RayGrid = RayGrid -- TODO: implement as array ??? (more safe, bound checking)
-- TODO (error checking) -> what if user defines resolution like (0,-1)
-- TODO: draw a proper image, describing the whole ray transformation pipeline

-- resolution told us, how many rays to create
castRays :: Camera -> Resolution -> Scene -> Image2Df
castRays (Camera (h,w) dist) res@(xRes,yRes) scene = pixels
    where rayDirs = [normalize $ toRay px py | px <- [0..xResF-1], 
                                               py <- [0..yResF-1]]

          pixels = computePixels res rays scene
          rays = map (\dir -> Ray (Vec3D 0 0 dist) dir) rayDirs

          toRay px py = Vec3D ((px / xResF - 0.5) * w)
                              ((py / yResF - 0.5) * h)
                              (-dist) -- z = distance

          (xResF, yResF) = (fromIntegral xRes, 
                            fromIntegral yRes)



-- Resolution => resolution of the grid of rays
computePixels :: Resolution -> [Ray] -> Scene -> Image2Df
computePixels res rays objects = 
              Image2Df res (map (computeColor objects) rays )



-- compute the color for the single ray
computeColor :: Scene -> Ray -> Color3f
computeColor objects ray@(Ray orig dir) = getColor fromTestRay
    where defaultColor = (0,0,0)
          fromTestRay  = findClosest objects ray
          getColor c = 
            case c of
                 Just (t, norm, SceneObject _ shader) -> let inter = orig + (t *> dir) 
                                                         in  shader inter norm
                 Nothing -> defaultColor


-- this shader just use material color, ignoring any light
simpleShader :: [LightSource] -> Material -> Position -> Normal -> Color3f
simpleShader _ mat _ _ = color mat


diffuseLightningShader :: [LightSource] -> Material -> Position -> Normal -> Color3f
diffuseLightningShader lights mat surfPos norm = foldl' addTuple (0,0,0) colorFromLights
    where colorFromLights = map calcColor lights
          toLight (LightSource lPos _) = normalize (lPos - surfPos)
          lnIntensity surfNorm toL     = clamp (toL `dot` surfNorm)
          finalColor  (r,g,b) intensity = (r * intensity,
                                           g * intensity,
                                           b * intensity)
          calcColor = finalColor (color mat) . lnIntensity norm . toLight



-- TODO -> draw a complete diagram of the processing
findClosest :: Scene -> Ray -> Maybe (Intersection, Normal, SceneObject)
-- alwayse choose the closest intersection point
findClosest objects r = 
  if (null intersections) then Nothing 
                          else Just (i, norm, obj)

    where intersections = filter ((/= Nothing) . fst) zipped
          zipped        = zip search objects
          search        = map (\(SceneObject geom _) -> intersect r geom) objects
          getT          = fst . fromJust . fst

          (Just (i,norm), obj) = L.minimumBy (compare `on` getT) intersections




-- global variables -------
width  = 300 :: Int
height = 300 :: Int


testMaterial1 = Material (0,1,1) 0 0 0
testMaterial2 = Material (1,0,0) 0 0 0

lightsList = [LightSource (Vec3D 0   10    10)  (  1,  1,  1),
              LightSource (Vec3D 0 (-10) (-10)) (0.5,0.5,0.5)]

testShader1   = diffuseLightningShader lightsList testMaterial1
testShader2   = diffuseLightningShader lightsList testMaterial2
testShader3   = simpleShader lightsList testMaterial2

imageToDevil img = convertToDevILFormat . convertImageUnsafe $ img
getTestImage = castRays (Camera (5.0,5.0) 5.0) (height, width)
                    [SceneObject (Sphere 2.0 (Vec3D   0   0 (-2))) testShader1,
                     SceneObject (Sphere 2.0 (Vec3D 2.0 2.0 (-2))) testShader3]


---------------------------


main :: IO ()
-- TODO: add time profiling
main = do
         ilInit -- initialize DevIL library
         let outFile = "test.png"
         -- create ouput image file
         exist <- doesFileExist outFile
         when exist $ removeFile outFile

         -- render image
         start  <- getCurrentTime
         forced <- evaluate (force getTestImage)
         stop   <- getCurrentTime
         
         -- write to file
         DEVIL.writeImage outFile (imageToDevil forced)
         putStrLn $ "Total: " ++ show (diffUTCTime stop start)


