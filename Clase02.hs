
{- comentario
multilínea -}

estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y | (x<=3) && (y<=3) = True
                      | (x>3 && x<=7) && (y>3 && y<=7) = True
                      | (x>7) && (y>7) = True
                      | otherwise = False

prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (vx, vy) (wx, wy) = vx*wx + vy*wy

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (vx, vy) (wx, wy) = (vx < wx) && (vy < wy)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (vx, vy) (wx, wy) = sqrt((vx-wx)**2 + (vy-wy)**2)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

-- comentario a ser ignorado
posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z) | mod x 2 == 0 = 1
                         | mod y 2 == 0 = 2
                         | mod z 2 == 0 = 3
                         | otherwise = 4

crearPar:: a -> b -> (a, b)
crearPar x y = (x, y)

{- | Documentacion invertir:
invertir toma un par y devuelve un par con el orden invertido -}
invertir :: (a, b) -> (b, a)
invertir (x, y) = (y, x)