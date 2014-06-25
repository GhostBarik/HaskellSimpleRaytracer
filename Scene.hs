-- file: Scene.hs
-- definition of scene objects and
-- function, working on them

-- additional extensions
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- export all stuff
module Scene where


import Math
import Image

import Data.Maybe

import System.IO.Unsafe

import qualified Prelude as P
import Prelude ((>), otherwise, min, undefined)
import NumericPrelude.Numeric



-- we definne some type synonyms for our convenience
type Position     = Vec3D Double
type Radius       = Double
type Normal       = Vec3D Double
type Intersection = Double -- t parameter
type Scene        = [SceneObject]



--data Material = Material {} 


-- Ray is defined by 2 vectors = Origin + Direction
data Ray = Ray (Vec3D Double) (Vec3D Double) deriving (P.Eq, P.Show)

----------------------------------

-- TODO: Renderable isntead of intersectable??
class GeometryObject a where 
    intersect :: Ray -> a -> Maybe (Intersection, Normal)

----------------------------------

-- Sphere => Radius + Position vector
data Sphere = Sphere Double (Vec3D Double)

instance GeometryObject Sphere where
    intersect (Ray orig dir) (Sphere rad cPos) = inters d

        where dest = orig - cPos
              b = dest `dot` dir
              c = dest `dot` dest - rad*rad
              d = b*b - c

              t1 = -b - (sqrt d)
              t2 = -b + (sqrt d)

              inters disc 
                  | disc > 0.0 = findT
                  | otherwise  = Nothing
              findT = Just ((min t1 t2), undefined)

-- | t1 > 0.0  = Just (t2, (Vec3D 0 1 0))
-- | otherwise = Just (t1, (Vec3D 0 1 0))
-----------------------------------

data Polygon = Polygon (Vec3D Double) (Vec3D Double) (Vec3D Double)

instance GeometryObject Polygon where
    intersect ray obj = undefined -- <- NOT IMPLEMENTED!

-----------------------------------

type ShaderFunc     = ([LightSource] -> Material -> Position -> Normal -> Color3f)
type CompiledShader = (Position -> Normal -> Color3f)

data Material = Material

-- simple light source for now   -- position -- color --
data LightSource = LightSource (Vec3D Double) (Color3f)

-- container for our object
-- (we use existential types here)
data SceneObject = forall a. GeometryObject a => SceneObject a CompiledShader 
