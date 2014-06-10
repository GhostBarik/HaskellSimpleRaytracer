import Data.List
import Codec.Image.DevIL

-- definition of the simple 3D-vector
data Vec3 a = Vec3 a a a deriving (Eq, Show)

(.+.) :: Num a => Vec3 a -> Vec3 a -> Vec3 a
(.+.) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1+x2) (y1+y2) (z1+z2)

(.*) :: Num a => a -> Vec3 a -> Vec3 a
(.*) scalar (Vec3 x y z) = Vec3 (scalar*x) (scalar*y) (scalar*z)

(*.) :: Num a => Vec3 a -> a -> Vec3 a
(*.) v scalar = scalar .* v  

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = (x1*x2)+(y1*y2)+(z1*z2)

len :: Fractional a => Vec3 a -> a
len v = sqrt $ (v `dot` v)

normalize :: Fractional a => Vec3 a -> Vec3 a
normalize v = v *. (1.0 / (len v))

-- TODO: discuss, why we couldn't just make our vector instance of Num


main :: IO ()
main = print (4 .* (Vec3 2 2 4) *. 2)

