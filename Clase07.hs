type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y    = True
                   | otherwise = pertenece x ys

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
            | otherwise = x:c

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = pertenece x c && incluido xs c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

union :: Set Int -> Set Int -> Set Int
union [] [] = []
union [] (y:ys) = agregar y (union [] ys)
union (x:xs) y = agregar x (union xs y) 

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] _ = []
interseccion (x:xs) y | pertenece x y = agregar x (interseccion xs y)
                      | otherwise = interseccion xs y

diferencia :: Set Int -> Set Int -> Set Int 
diferencia [] _ = []
diferencia (x:xs) y | not (pertenece x y) = agregar x (diferencia xs y)
                    | otherwise = diferencia xs y

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica c1 c2 = diferencia (union c1 c2) (interseccion c1 c2)

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC xs xss | xs `perteneceC` xss = xss
                | otherwise = xs:xss

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC xs [] = False
perteneceC xs (ys:yss) = iguales xs ys || perteneceC xs yss

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int) 
agregarATodos x [] = []
agregarATodos x (c:cs) = agregarC (agregar x c) (agregarATodos x cs)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (x:xs) = unionC (partes xs) (agregarATodos x (partes xs))

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] [] = []
unionC [] (y:ys) = agregarC y (unionC [] ys)
unionC (x:xs) y = agregarC x (unionC xs y) 

partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = unionC (partesN (n-1)) (agregarATodos n (partesN (n-1)))

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] _ = []
productoCartesiano _ [] = []
productoCartesiano xs (y:ys) = unionT (tuplasConjuntoN xs y) (productoCartesiano xs ys)

tuplasConjuntoN :: Set Int -> Int -> Set (Int, Int)
tuplasConjuntoN [] n = []
tuplasConjuntoN (x:xs) n = agregarT (x, n) (tuplasConjuntoN xs n)

agregarT :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
agregarT xs xss | xs `perteneceT` xss = xss
                | otherwise = xs:xss

perteneceT :: (Int, Int) -> Set (Int, Int) -> Bool
perteneceT _ [] = False
perteneceT (x1,x2) ((y1,y2):ys) | x1 == y1 && x2 == y2    = True
                   | otherwise = perteneceT (x1,x2) ys

unionT :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
unionT [] [] = []
unionT [] (y:ys) = agregarT y (unionT [] ys)
unionT (x:xs) y = agregarT x (unionT xs y) 