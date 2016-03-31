-- file: Main.hs
-- math functions and datatypes

 {-# LANGUAGE MultiParamTypeClasses#-}
 {-# LANGUAGE FlexibleInstances#-}


-- export all stuff
module Math where

import qualified NumericPrelude.Numeric()
import qualified Algebra.Additive
import qualified Algebra.FloatingPoint

import Algebra.Module (C, (*>))

-- definition of the simple 3D-vector
data Vec3D a = Vec3D a a a deriving (Eq, Show)

instance (Floating a) => Algebra.Additive.C (Vec3D a) where
    zero = Vec3D 0 0 0
    (+) (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D (x1 + x2) (y1 + y2) (z1 + z2)
    (-) (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D (x1 - x2) (y1 - y2) (z1 - z2)
    negate (Vec3D x y z) = Vec3D (-x) (-y) (-z)

instance (Algebra.FloatingPoint.C a, Floating a) => Algebra.Module.C (a) (Vec3D a) where
    (*>) scalar (Vec3D x y z) = Vec3D (scalar * x) (scalar * y) (scalar * z)

(<*) :: Algebra.Module.C a v => v -> a -> v
(<*) = flip (*>) 

dot :: (Floating a) => Vec3D a -> Vec3D a -> a
dot (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

len :: (Floating a) => Vec3D a -> a
len v = sqrt (v `dot` v)

normalize :: (Floating a) => Vec3D a -> Vec3D a
normalize v@(Vec3D x y z) = let l = len v in
                            	    Vec3D (x/l) (y/l) (z/l)

cross :: (Floating a) => Vec3D a -> Vec3D a -> Vec3D a
cross (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D x3 y3 z3
    where x3 = y1 * z2 - z1 * y2
          y3 = z1 * x2 - x1 * z2
          z3 = x1 * y2 - y1 * x2

-- AUXILIARY FUNCTIONS

mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3  f (x,y,z)= (f x, f y, f z)

clamp :: (Floating a, Ord a) => a -> a
clamp value 
    | value <= 0.0 = 0.0
    | value >= 1.0 = 1.0
    | otherwise = value
