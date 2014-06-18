-- Main.hs

module Main 
(
  main
) 
where

import Math
import Image

import Data.Function
import Data.Maybe
import Control.Monad

import Data.List          as L

import System.Directory
import Codec.Image.DevIL as DEVIL



-- (height!, width!) <- right format
type ScreenSize = (Double, Double) -- size of the scene camera's screen in relative units
type Distance = Double -- screen offset from the origin of axis


type Position     = Vec3D Double
type Radius       = Double
type Normal       = Vec3D Double
type Intersection = Double -- t parameter

type Scene = [SceneObject]

--data RayGrid = RayGrid -- TODO: implement as array ??? (more safe, bound checking)


data Camera = Camera ScreenSize Distance

data Ray = Ray {  getRayOrigin    :: Vec3D Double,
                  getRayDirection :: Vec3D Double  } deriving (Eq, Show)

-- scene objects
data SceneObject = Polygon (Vec3D Double) (Vec3D Double) (Vec3D Double) | 

                   Sphere { getSphereRaduis   :: Double,
                            getSpherePosition :: Vec3D Double}

                   deriving (Eq, Show)

-- for now use default material

-- very simple material (only color without transparency)
-- TODO: should be composed with object???
data Material = Material Color3f

-- TODO (error checking) -> what if user defines resolution like (0,-1)
-- TODO: draw a proper image, describing the whole ray transformation pipeline


-- resolution told us, how many rays to create
castRays :: Camera -> Resolution -> Scene -> Image2Df
castRays (Camera (h,w) dist) res@(xRes,yRes) scene = pixels

    where rayDirs = [toRay px py | px <- [0..xResF-1], 
                                   py <- [0..yResF-1]]

          pixels = computePixels res rays scene
          rays = map (\dir -> Ray (Vec3D 0 0 0) dir) rayDirs

          toRay px py = Vec3D ((px / xResF - 0.5) * w) 
                              ((py / yResF - 0.5) * h)
                              (dist) -- z = distance

          (xResF, yResF) = (fromIntegral xRes, 
                            fromIntegral yRes)



-- Resolution => resolution of the grid of rays
computePixels :: Resolution -> [Ray] -> Scene -> Image2Df
computePixels res rays scene = 
            Image2Df res (map (computeColor scene) rays)


-- intersection :: Ray -> [Objects] -> (PointOfIntersection, Normal)
-- intersectionToSphere :: Ray -> Sphere -> Maybe (normal, pointOfIntersection)
-- intersectionPoly .....
-- intersection....




-- compute the color for the single ray
computeColor :: Scene -> Ray -> Color3f
computeColor scene r = getColor (findClosestIntersection scene r)
      where getColor (Just _) = (1,1,1)-- hit! white color only for test purposes!
            getColor Nothing  = defaultColor
            defaultColor      = (0,0,0)


findClosestIntersection :: Scene -> Ray -> Maybe (Intersection, Normal)
-- alwayse choose the closest intersection point
findClosestIntersection objects r = if null intersections then 
                                      Nothing 
                                    else 
                                      Just closest

      where intersections = catMaybes search
            -- map fromJust . filter (/= Nothing) $ search (old version, good to compare)
            search  = map (findIntersection r) $ objects
            closest = L.minimumBy (compare `on` fst) intersections


findIntersection :: Ray -> SceneObject -> Maybe (Intersection, Normal)
findIntersection r (Sphere rad pos) = Just (5.0, Vec3D 0 1 0) -- <- 5.0 only for test!!
findIntersection r _                = Nothing


-- TODO: make this with typeclasses
intersectSphere :: Ray -> Position -> Radius -> Maybe (Intersection, Normal)
intersectSphere r p rad = undefined






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
                      castRays (Camera (2.0,2.0) 1.0) (height, width) []

---------------------------



                                    



                                                                          



main :: IO ()
--main = print 

main = do
         ilInit -- initialize DevIL library
         let outFile = "test.png"
         exist <- doesFileExist outFile
         when exist $ removeFile outFile
         DEVIL.writeImage outFile testImg2


