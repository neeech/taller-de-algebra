type Posicion = [Int]
type Jugada = (Int, Int)


{- Ej 1 recibe una posición p, una jugada válida j y devuelve la posición obtenida al
realizar dicha jugada. -}
jugar :: Posicion -> Jugada -> Posicion 
jugar p j | fst j == 1 = quitarCeroSiHay(head p - snd j : tail p)  -- Una vez hallada la pila, le resta la cantidad de piedras de la jugada y copia el resto de la posición 
          | otherwise = head p : jugar (tail p) (fst j - 1, snd j) -- Copia cada pila de la posición que no sea la pila que indica la jugada.
          --Hace la recursión sobre la cola de posición restándole 1 a la primera componente de la jugada

quitarCeroSiHay :: Posicion -> Posicion
quitarCeroSiHay (x:xs) | x == 0 = xs -- Si la cabeza de la posición es un 0, lo quita.
                       | otherwise = (x:xs)

-- Ej 2 recibe una posición p y devuelve el conjunto de jugadas válidas a partir de p
posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas p = posiblesJugadasDesde 1 p

posiblesJugadasDesde :: Int -> Posicion -> [Jugada]
posiblesJugadasDesde _ [] = [] -- No hay jugadas posibles
posiblesJugadasDesde n p = union (posiblesJugadasEn n (head p)) (posiblesJugadasDesde (n+1) (tail p)) -- n será la primera componente de cada jugada y el máximo de piedras será el número correspondiente en la pila n de p

posiblesJugadasEn :: Int -> Int -> [Jugada]
posiblesJugadasEn _ 0 = []
posiblesJugadasEn n c = (n, c) : posiblesJugadasEn n (c-1) -- Forma las jugadas con cada tupla posible donde n es fijo y 1<=c<=c

union :: [Jugada] -> [Jugada] -> [Jugada]
union [] ys     = ys
union (x:xs) ys = union xs (x:ys) -- No considera elementos repetidos porque PosiblesJugadasDesde no los forma

-- Ej 3 decide si una posición p es ganadora
esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p = esPosicionGanadoraConJugadas p (posiblesJugadas p)  -- Utiliza una función auxiliar que contenga las posibles jugadas

esPosicionGanadoraConJugadas :: Posicion -> [Jugada] -> Bool
esPosicionGanadoraConJugadas p j | j == [] = False -- No hay jugadas posibles
                                 | otherwise = not (esPosicionGanadoraConJugadas (resultado) (posiblesJugadas(resultado))) || -- Si resultado es posicion no ganadora devuelve True
                                  esPosicionGanadoraConJugadas p (tail j) -- Hace la recursión con las jugadas restantes de las posibles
                                  where resultado = jugar p (head j) -- Aplica la primera jugada de las posibles sobre la posición actual
                                

{- Ej 4 recibe una posición ganadora p y devuelve una jugada que dejaría al rival en
una posición no ganadora. -}
jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p = jugadaGanadoraDeLasPosibles p (posiblesJugadas p)

jugadaGanadoraDeLasPosibles :: Posicion -> [Jugada] -> Jugada
jugadaGanadoraDeLasPosibles p j | not (esPosicionGanadora resultado) = head j -- Si luego de aplicar una jugada sobre p el resultado es una posición no ganadora, devuelve esta jugada
                                | otherwise = jugadaGanadoraDeLasPosibles p (tail j) -- Continúa sobre el resto de las jugadas
                                where resultado = jugar p (head j)

{- Recibe una posición p (no necesariamente ganadora) y devuelve la cantidad de
jugadas ganadoras partiendo de p.
-}
numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p | not (esPosicionGanadora p) = 0  -- Si es posición ganadora, no tiene jugadas ganadoras
                           | otherwise = numeroDeJugadasGanadorasConJugadas p (posiblesJugadas p)

numeroDeJugadasGanadorasConJugadas :: Posicion -> [Jugada] -> Int
numeroDeJugadasGanadorasConJugadas _ [] = 0 -- No hay jugadas posibles
numeroDeJugadasGanadorasConJugadas p j | not (esPosicionGanadora resultado) = 1 + numeroDeJugadasGanadorasConJugadas p (tail j) -- Si luego de aplicar una jugada sobre p el resultado es una posición no ganadora, suma 1 y se lo suma a la función sin considerar esta jugada
                                       | otherwise = numeroDeJugadasGanadorasConJugadas p (tail j) -- No suma y hace la recursión sin considerar la jugada
                                       where resultado = jugar p (head j) -- Aplica la primera jugada posible a la posición inicial