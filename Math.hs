-- file: Main.hs
-- math functions and datatypes

-- import all stuff
module Math where


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

-- TODO: discuss, why we couldn't just make our vector instance of Num

-- not tested yet!!! need testing!
cross :: Floating a => Vec3D a -> Vec3D a -> Vec3D a
cross (Vec3D x1 y1 z1) (Vec3D x2 y2 z2) = Vec3D x3 y3 z3
    where x3 = y1 * z2 - z1 * y2
          y3 = z1 * x2 - x1 * z2
          z3 = x1 * y2 - y1 * x2


-- auxiliary functions
mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3  f (x,y,z)= (f x, f y, f z)


