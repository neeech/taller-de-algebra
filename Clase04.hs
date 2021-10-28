g1 :: Int -> Int -> Int
g1 i n | i > n = 0
       | otherwise =  g1 i (n-1) + i^n

g2 :: Int -> Int
g2 n = g5 n n

g3 :: Int -> Int
g3 n | n == 0 = 0
     | mod n 2 == 1 = g3 (n-1)
     | otherwise = g3 (n-2) + 2^n

g4 :: Int -> Int
g4 n | n == 0 = 0
     | digitosIguales n = n + g4 (n-1)
     | otherwise = g4 (n-1)

digitosIguales :: Int -> Bool
digitosIguales n | n < 10 = True
                 | otherwise = digitosIguales (div n 10) && mod n 10 == mod (div n 10) 10

g5 :: Int -> Int -> Int
g5 0 _ = 0
g5 n m = g5(n-1) m + g1 n m
    