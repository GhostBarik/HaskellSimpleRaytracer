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

import Data.List as L hiding (intersect)

import System.Directory
import Codec.Image.DevIL as DEVIL

import Data.Array.IArray  as IA


-- (height!, width!) <- right format
type ScreenSize = (Double, Double) -- size of the scene camera's screen in relative units
type Distance = Double -- screen offset from the origin of axis


--data RayGrid = RayGrid -- TODO: implement as array ??? (more safe, bound checking)

data Camera = Camera ScreenSize Distance

-- for now use default material

-- very simple material (only color without transparency)
-- TODO: should be composed with object???
data Material = Material Color3f

-- TODO (error checking) -> what if user defines resolution like (0,-1)
-- TODO: draw a proper image, describing the whole ray transformation pipeline


-- resolution told us, how many rays to create
castRays :: Camera -> Resolution -> Scene -> (Image2Df, [Ray])
castRays (Camera (h,w) dist) res@(xRes,yRes) scene = (pixels, rays)

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
            Image2Df res (map (computeColor objects) rays)


-- compute the color for the single ray
computeColor :: Scene -> Ray -> Color3f
computeColor objects r = getColor (findClosest objects r)
    where defaultColor = (0,0,0)
          getColor c = case c of
                            (Just (t,_,_)) -> (1,1,1) -- hit! white color only for test purposes!
                            Nothing -> defaultColor
                                    

--                       1               2                     3
-- generic shader :: (Material) -> ([LightSources]) -> (posistion -> normal) -> Color
 
-- object <-ONE-> material <-ONE-> shader
-- object knows nothing about light sources in the scene
-- object has material
-- objects has shader
-- shader use material ("compiled shader" remembers its material)

-- intersection -> POINT -> shade this point -> Color

-- object = geometry + (material+shader func = compiled shader!!)

findClosest :: Scene -> Ray -> Maybe (Intersection, Normal, SceneObject)
-- alwayse choose the closest intersection point
findClosest objects r = 
  if (null intersections) then Nothing 
                          else Just closest
      -- map fromJust . filter (/= Nothing) $ search (old version, good to compare)
      where intersections = catMaybes search
            search  = map (\(SceneObject o _) -> (intersect r o, 0)) objects
            closest = L.minimumBy (compare `on` fst) intersections
            fst (a,_,_) = a











-- global variables -------
width  = 100 :: Int
height = 100 :: Int

--testImage :: Image2Df
--testImage = Image2Df (width, height) 
--                     [mapTuple3 pixFunc (x,y,200) | y <- [0..height-1], 
--                                                    x <- [0..width-1]]
--    where pixFunc p = (fromIntegral p :: Double) / 10.0
--    -- TODO: dont't use literal 400.0 !! unsafe
--    -- better make a choice betwee 2 values

--testDevImg1 = convertToDevILFormat . convertImageUnsafe $ testImage

testImg2    = ((convertToDevILFormat . convertImageUnsafe $ img), img)
  where (img, rs) = castRays (Camera (5.0,5.0) 5.0) (height, width)
                             [SceneObject (Sphere 2.0 (Vec3D 0 0 (-2))) undefined]
                      

---------------------------



                                    



                                                                          



main :: IO ()
--main = print 

main = do
         ilInit -- initialize DevIL library
         let outFile = "test.png"
         exist <- doesFileExist outFile
         when exist $ removeFile outFile
         let (img, rs) = testImg2
         let Image2Df _ pixels = rs 
         --mapM print pixels
         DEVIL.writeImage outFile img


