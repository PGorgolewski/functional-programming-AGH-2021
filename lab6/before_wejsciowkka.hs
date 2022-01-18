import Prelude (Applicative)
tryFactorial :: Int -> Maybe Int
tryFactorial 0 = Just 1
tryFactorial n =
    if n < 0 then Nothing 
    else do
        prev <- tryFactorial $ n - 1
        return $ n * prev

-- return daje nam już Maybe, robiąć return (Just ..) dostajemy Maybe (Maybe x)

data [] a = [] | a : [a]

instance Applicative (Applicative a => [a]) where
    pure x = [x]
    fs <*> xs = [ f x | f <- fs, x <- xs]