--Napisać wyrażenie obliczające, ile jest w przedziale
--[1,100] trójek liczb całkowitych reprezentujących 
--długości boków trójkąta prostokątnego


-- let GetPitagorasTriangle = [(a,b,c) | a <- [1..100], b <- [1..100], c <- [1..100], a^2 + b^2 == c^2]

-- Czy poniższa definicja funkcji (sprawdzającej, czy liczba jest pierwsza) jest poprawna?
isPrime :: Integral t => t -> Bool
isPrime n = null ([i | i <- [2..n-1], n `mod` i == 0])

