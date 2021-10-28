--------------------------------------------------------------------------------
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

type Set a = [a]

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)

--------------------------------------------------------------------------------
---------------------------- Funciones Clase 8 ---------------------------------
--------------------------------------------------------------------------------
combinatorio :: Int -> Int -> Int
combinatorio n k = (fact n) `div` ((fact k) * (fact (n-k)))

combinatorio' :: Int -> Int -> Int
combinatorio' n 0 = 1
combinatorio' n k | n == k = 1
                  | otherwise = (combinatorio' (n-1) k ) + (combinatorio' (n-1) (k-1))

---------------------------------------------------------------------------------
agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante x [] = []
agregarElementoAdelante x (ys:yss) = agregar (x:ys) (agregarElementoAdelante x yss)

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _ = []
agregarElementosAListas (x:xs) c = union (agregarElementoAdelante x c) (agregarElementosAListas xs c)

variaciones :: Set Int -> Int -> Set [Int]
variaciones c 0 = [[]]
variaciones c k = agregarElementosAListas c (variaciones c (k-1))
---------------------------------------------------------------------------------
insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn xs n i | i == 1 = n:xs
                  | otherwise = (head xs) : (insertarEn (tail xs) n (i-1))

insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPos xs c 1 = agregar (insertarEn xs c 1) vacio
insertarEnCadaPos xs c i = agregar (insertarEn xs c i) (insertarEnCadaPos xs c (i-1))


insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] c = []
insertarEnCadaPosDeTodasLasListas (xs:xss) c = (insertarEnCadaPos xs c (length xs +1)) `union`
                                               (insertarEnCadaPosDeTodasLasListas xss c) 

permutaciones :: Set Int -> Set [Int]
permutaciones [] = [[]]            
permutaciones (c:cs) = insertarEnCadaPosDeTodasLasListas (permutaciones cs) c
---------------------------------------------------------------------------------
-- String = [Char] 'a' 'h':"ola"
bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas k c = variaciones ([1..c]) k

bolitasEnCajas' :: Int -> Int -> Set [Int]
bolitasEnCajas' k c = insertarEnCadaPosDeTodasLasListas (variaciones ([1..c]) (k-1)) 1

----------------------------------------------------------------------------
ordenadomin :: [Int] -> Bool
ordenadomin (x:xs) | xs == [] = True
                | otherwise = (minimo (x:xs) == x) && ordenadomin xs 

ordenadomax :: [Int] -> Bool
ordenadomax (x:xs) | xs == [] = True
                | otherwise = (maximo (x:xs) == x) && ordenadomax xs 

minimo :: [Int] -> Int 
minimo l | tail l == [] = head l
         | head l < minimo (tail l) = head l
         | otherwise = minimo (tail l)

maximo :: [Int] -> Int 
maximo l | tail l == [] = head l
         | head l > maximo (tail l) = head l
         | otherwise = maximo (tail l)

listasOrdenadas :: Int -> Int -> Set [Int]
listasOrdenadas k n = quitarNoOrdenados (variaciones ([1..n]) k)

quitarNoOrdenados :: Set [Int] -> Set [Int]
quitarNoOrdenados [] = []
quitarNoOrdenados (xs:xss) | (ordenadomax xs) || (ordenadomin xs) = xs : quitarNoOrdenados xss
                           | otherwise = quitarNoOrdenados xss