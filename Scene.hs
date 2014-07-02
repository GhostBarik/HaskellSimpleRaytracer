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
import Prelude ((>), (<), (==), (||), otherwise, min, undefined)
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

-- TODO: data constructor should be hidden
data Triangle = Triangle (Vec3D Double) 
                         (Vec3D Double)
                         (Vec3D Double)
                         Normal

makeTriangle :: (Double, Double, Double) 
             -> (Double, Double, Double)
             -> (Double, Double, Double) 
             -> Triangle

makeTriangle (a1,b1,c1) (a2,b2,c2) (a3,b3,c3) = result
    where v1 = Vec3D a1 b1 c1
          v2 = Vec3D a2 b2 c2
          v3 = Vec3D a3 b3 c3
          v21 = normalize (v2 - v1)
          v31 = normalize (v3 - v1)
          surfNormal = normalize (cross v31 v21)
          result = Triangle v1 v2 v3 surfNormal


instance GeometryObject Triangle where
    intersect (Ray orig dir) (Triangle v1 v2 v3 norm) = result
        where e1 = v2 - v1
              e2 = v3 - v1
              d  = orig - v1

              s1 = dir `cross` e2
              s2 = d `cross` e1
              divisor = s1 `dot` e1
              invDivisor = 1.0 / divisor

              b1 = (d   `dot` s1) <* invDivisor
              b2 = (dir `dot` s2) <* invDivisor
              t  = (e2  `dot` s2) <* invDivisor

              result = if divisor == 0.0 ||
                          b1 < 0.0 || b1 > 1.0 ||
                          b2 < 0.0 || (b1 + b2) > 1.0
                       then
                          Nothing
                       else
                          Just (t, norm)

              


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
