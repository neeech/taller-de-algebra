--ej 1
satisfaceGoldbach :: Integer -> Bool -- recibe un número natural n
satisfaceGoldbach n | not ((mod n 2 == 0) && n > 2) = False -- n debe ser par y mayor que 2
                    | otherwise = satisfaceGoldbachIter n 2 (n-2)

satisfaceGoldbachIter :: Integer -> Integer -> Integer -> Bool
satisfaceGoldbachIter n i j | j < 2 = False -- cuando se completan todas las sumas iguales a n y ninguna involucra 2 primos
                            | esPrimo i && esPrimo j = True -- hay al menos una suma que satisface verifica que ambos sean primos
                            | otherwise = satisfaceGoldbachIter n (i+1) (j-1) -- la suma i + j siempre será n

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n 

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise    = menorDivisorDesde n (k+1)

--ej 2
verificarConjeturaHasta :: Integer -> Bool -- recibe un número natural n par mayor que 2
verificarConjeturaHasta n | n == 4 = satisfaceGoldbach 4 
                          | otherwise = verificarConjeturaHasta (n-2) && satisfaceGoldbach n

--ej 3
descomposicionEnPrimos :: Integer -> (Integer,Integer) -- recibe un número natural n par mayor que 2
descomposicionEnPrimos n = descomposicionEnPrimosIter n 2 (n-2)

descomposicionEnPrimosIter :: Integer -> Integer -> Integer -> (Integer,Integer)
descomposicionEnPrimosIter n i j | esPrimo i && esPrimo j = (i, j) -- devuelve la primera descomposición que encuentre
                                 | otherwise = descomposicionEnPrimosIter n (i+1) (j-1)

--ej 4
numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n = numeroDeDescomposicionesIter n 2 (n-2)

numeroDeDescomposicionesIter :: Integer -> Integer -> Integer -> Integer -- recibe un número natural n par mayor que 2
numeroDeDescomposicionesIter n i j | j < 2 = 0 -- se completan todas las sumas iguales a n entre 2 y n-2
                                   | esPrimo i && esPrimo j = 1 + numeroDeDescomposicionesIter n (i+1) (j-1)
                                   | otherwise = numeroDeDescomposicionesIter n (i+1) (j-1)