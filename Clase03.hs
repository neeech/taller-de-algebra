multiploDe3 :: Int -> Bool
multiploDe3 n | n == 0 = True
              | n == 1 = False
              | n == 2 = False
              | otherwise = multiploDe3 (n-3)

sumaImpares :: Int -> Int
sumaImpares n | n == 1 = 1
              | otherwise = sumaImpares (n-1) + 2*n - 1

medioFact :: Int -> Int
medioFact n | n == 0 = 1
            | n == 1 = 1
            | otherwise = n * medioFact (n-2)

sumaDigitos :: Int -> Int
sumaDigitos n | n == 0 = 0
              | otherwise = sumaDigitos(div n 10) + mod n 10

digitosIguales :: Int -> Bool
digitosIguales n | n < 10 = True
                 | otherwise = digitosIguales (div n 10) && mod n 10 == mod (div n 10) 10