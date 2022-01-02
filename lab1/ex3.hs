import Distribution.Simple.Utils (xargs)
sgn :: Int -> Int
sgn n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1


absInt :: Int -> Int -- absInt 2 = absInt (-2) = 2
absInt x = if x>= 0
          then x
          else -x
    


not' :: Bool -> Bool
not' b = case b of
          True  -> False
          False -> True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True -- :)
isItTheAnswer _      = False


roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where {
      d = sqrt (b * b - 4 * a * c)
      e = 2 * a -- uwaga na przesunięcie!
   }
   