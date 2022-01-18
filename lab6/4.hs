xs1 :: [(Int,Int,Int)]
xs1 = [ (x,y,z) | let xs = [1,2], x <- xs, y <- xs, z <- xs ]

doXs1 :: [(Int,Int,Int)]
doXs1 = do
  let xs = [1,2]
  x <- xs
  y <- xs
  z <- xs
  return (x,y,z)