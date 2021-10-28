sumatoria :: [Int] -> Int
sumatoria l | l == [] = 0
            | otherwise = head l + sumatoria (tail l)

longitud :: [Int] -> Int
longitud l | l == [] = 0
           | otherwise = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | otherwise = (x == head l) || pertenece x (tail l)

primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 l | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiplode45345 (tail l)

sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

longitudPM :: [a] -> Int
longitudPM [] = 0
longitudPM (_:xs) = 1 + longitudPM xs

pertenecePM :: (Eq a) => a -> [a] -> Bool
pertenecePM _ [] = False
pertenecePM x (h:t) = (x == h) || pertenecePM x t

productoria :: [Int] -> Int
productoria l | l == [] = 1
              | otherwise = head l * productoria (tail l)

sumarN :: Int -> [Int] -> [Int]
sumarN n xs | xs == [] = []
            | otherwise = (n + head xs) : (sumarN n (tail xs))

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs = sumarN (head xs) xs

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs = sumarN ultimo xs
              where ultimo = ultimoElemento xs

ultimoElemento :: [Int] -> Int
ultimoElemento xs | (tail xs == []) = head xs
                  | otherwise = ultimoElemento (tail xs)

pares :: [Int] -> [Int]
pares l | l == [] = []
        | (mod (head l) 2 == 0) = head l : pares (tail l)
        | otherwise = pares (tail l)

quitar :: Int -> [Int] -> [Int]
quitar n l | l == [] = []
           | not (head l == n) = head l : quitar n (tail l)
           | otherwise = tail l

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas n l | l == [] = []
                | not (head l == n) = head l : quitarTodas n (tail l)
                | otherwise = quitarTodas n (tail l)

hayRepetidos :: [Int] -> Bool
hayRepetidos l | tail l == [] = False
               | otherwise = pertenece (head l) (tail l) || hayRepetidos (tail l)


eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal l | l == [] = []
                           | not (pertenece (head l) (tail l)) = head l : eliminarRepetidosAlFinal (tail l)
                           | otherwise = head l : eliminarRepetidosAlFinal (quitarTodas (head l) (tail l))

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio l | l == [] = []
                           | not (pertenece (head l) (tail l)) = head l : eliminarRepetidosAlInicio (tail l)
                           | otherwise = eliminarRepetidosAlInicio (tail l)              

maximo :: [Int] -> Int 
maximo l | tail l == [] = head l
         | head l > maximo (tail l) = head l
         | otherwise = maximo (tail l)

minimo :: [Int] -> Int 
minimo l | tail l == [] = head l
         | head l < minimo (tail l) = head l
         | otherwise = minimo (tail l)

ordenar :: [Int] -> [Int]
ordenar l | l == [] = []
          | otherwise = minimo l : ordenar (quitar (minimo l) l)

reverso :: [Int] -> [Int]
reverso l | l == [] = []
          | otherwise = ultimoElemento l : reverso (quitarUltimo l)

quitarUltimo :: [Int] -> [Int]
quitarUltimo l | tail l == [] = []
               | otherwise = head l : quitarUltimo (tail l)

concatenar :: [Int] -> [Int] -> [Int]
concatenar l1 l2 | l1 == [] = l2
                 | otherwise = head l1 : concatenar (tail l1) l2

zipi :: (Eq a, Eq b) => [a] -> [b] -> [(a,b)] 
zipi l1 l2 | (l1 == [] || l2 == []) = []
           | otherwise = (head l1, head l2) : zipi (tail l1) (tail l2)