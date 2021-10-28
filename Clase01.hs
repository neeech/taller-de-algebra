absoluto :: Int -> Int
absoluto x | x < 0 = (-1)*x
           | otherwise = x

maximoabsoluto :: Int -> Int -> Int 
maximoabsoluto x y | absoluto x >= absoluto y = absoluto x
                   | otherwise = absoluto y

maximo3 :: Int -> Int -> Int -> Int 
maximo3 x y z | (x >= y) && (x >= z) = x
              | (y > x) && (y >= z) = y
              | otherwise = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x == 0 || y == 0 = True 
              | otherwise = False 

algunoEs0b :: Float -> Float -> Bool
algunoEs0b 0 _ = True  
algunoEs0b _ 0 = True
algunoEs0b _ _ = False 

algunoEs0c :: Float -> Float -> Bool
algunoEs0c x y = (x == 0 || y == 0)

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False 

ambosSon0b :: Float -> Float -> Bool
ambosSon0b 0 0 = True 
ambosSon0b _ _ = False

esMultiploDe :: Int -> Int -> Bool 
esMultiploDe x y | mod x y == 0 = True 
                 | otherwise = False 

digitoUnidades :: Int -> Int 
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int 
digitoDecenas x = mod (div x 10) 10





