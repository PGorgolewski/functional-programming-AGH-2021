sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x,y,z) = sqrt(x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (x, str) = (str, x)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = x == y && x == z

heron :: (Double, Double, Double) -> Double 
heron (a,b,c) = let p = (a+b+c)/2 in sqrt(p*(p - a)*(p - b)*(p - c))
