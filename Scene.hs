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

import qualified Prelude as P
import Prelude ((>), otherwise, min, undefined)
import NumericPrelude.Numeric



-- we definne some type synonyms for our convenience
type Position     = Vec3D Double
type Radius       = Double
type Normal       = Vec3D Double
type Intersection = Double -- t parameter
type Scene        = [SceneObject]



-- Ray is defined by 2 vectors = Origin + Direction
data Ray = Ray (Vec3D Double) (Vec3D Double) deriving (P.Eq, P.Show)

----------------------------------

class GeometryObject a where 
    intersect :: Ray -> a -> Maybe (Intersection, Normal)

----------------------------------

-- Sphere => Radius + Position vector
data Sphere = Sphere Double (Vec3D Double)

instance GeometryObject Sphere where
    intersect (Ray orig dir) (Sphere rad cPos) = inters d

        where dest = orig - cPos
              b    = dest `dot` dir
              c    = dest `dot` dest - rad*rad
              d    = b*b - c

              t1 = -b - sqrt d
              t2 = -b + sqrt d
              t  = min t1 t2

              inters disc 
                  | disc > 0.0 = findT
                  | otherwise  = Nothing

              pointOfInters = orig + (t *> dir)
              normal = normalize (pointOfInters - cPos) 
              findT = Just (t, normal)

-----------------------------------

data Polygon = Polygon (Vec3D Double) (Vec3D Double) (Vec3D Double)

instance GeometryObject Polygon where
    intersect _ _ = undefined -- <- NOT IMPLEMENTED!

-----------------------------------

-- container for our object
-- (we use existential types here)
data SceneObject = forall a. GeometryObject a => SceneObject a CompiledShader
-----------------------------------

type ShaderFunc     = ([LightSource] -> Material -> Position -> Normal -> Color3f)
type CompiledShader = (Position -> Normal -> Color3f)

data Material = Material { color :: Color3f,     -- surface color
                           kDiff :: Double,      -- diffuse intensity koefficient
                           kSpec :: Double,      -- specular instensity
                           shininess :: Double } -- shininess

-- simple light source for now   -- position -- color --
data LightSource = LightSource Position Color3f
