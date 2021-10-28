type Set a = [a]
-- | Division de numeros naturales_0 : a `divNat ` d = a `div` d
divNat :: Int -> Int -> Int
divNat a d | a < d = 0
           | otherwise = (a - d) `divNat` d + 1

-- | Resto de numeros naturales_0 : a `modNat` d = a `mod` d
modNat :: Int -> Int -> Int
modNat a d = a - d *(a `divNat` d)

-- | Modulo de numeros enteros a `modulo` d = a `mod` d
modulo :: Int -> Int -> Int
modulo a d | a >= 0 || r' == 0 = r'
           | otherwise = abs d - r'
    where r' = abs a `modNat` abs d

-- | Division de numeros enteros : n `dividido` m = n `div` m
dividido :: Int -> Int -> Int
dividido a d = sgq * absq -- obs 1
    where absq = abs (a -r) `divNat` (abs d ) -- obs 2
          sgq = ( signum a ) * ( signum d) -- obs 3
          r = a `modulo` d

--ej 1
digitos :: Integer -> Integer -> [Integer] 
digitos 0 _ = []
digitos n b = (mod n b) : digitos (div n b) b

--ej 2
numero :: [Integer] -> Integer -> Integer
numero l b = numeroDesde l b 0

numeroDesde :: [Integer] -> Integer -> Integer -> Integer
numeroDesde [] _ _ = 0 
numeroDesde (x:xs) b n = x * b^n + numeroDesde xs b (n+1)

--ej 3
divisores :: Int -> Set Int
divisores n = divisoresDesde 1 n

divisoresDesde :: Int -> Int -> Set Int
divisoresDesde i n | (i > (div n 2)) = [n]
                   | mod n i == 0 = i : divisoresDesde (i+1) n
                   | otherwise = divisoresDesde (i+1) n

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] _ = []
interseccion (x:xs) y | pertenece x y = agregar x (interseccion xs y)
                      | otherwise = interseccion xs y

maximo :: [Int] -> Int 
maximo l | tail l == [] = head l
         | head l > maximo (tail l) = head l
         | otherwise = maximo (tail l)

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
            | otherwise = x:c

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y    = True
                   | otherwise = pertenece x ys

mcdDef :: Int -> Int -> Int
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximo ( interseccion ( divisores a) ( divisores b ) )

--ej 6
mcd :: Int -> Int -> Int
mcd a b | abs b > abs a = mcd b a
mcd a 0 = abs a
mcd a b = mcd b (mod a b)

--ej 8
mcm :: Int -> Int -> Int
mcm a b = abs(a*b) `div` mcd a b

--ej 9
emcd :: Int -> Int -> (Int, Int, Int)
emcd a b | b > a = emcd b a
emcd a 0 = (a, 1, 0)
emcd a b = (d, t, s - t * k)
    where (k, r) = (div a b, mod a b)
          (d, s, t) = emcd b r
