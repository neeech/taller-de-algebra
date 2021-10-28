fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)
--7
mayorFactHasta :: Int -> Int
mayorFactHasta m = mayorFactHastaDesde 1 m

mayorFactHastaDesde :: Int -> Int -> Int
mayorFactHastaDesde i m | (fact i) > m = fact (i-1) 
                        | otherwise     = mayorFactHastaDesde (i+1) m 
--8
esFact :: Int -> Bool
esFact n = (n == (mayorFactHasta n))
---------------------------------------------------------------------
fibonacci :: Int -> Int
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

mayorFibonacciHasta :: Int -> Int
mayorFibonacciHasta n = mayorFibonacciHastaDesde 1 n

mayorFibonacciHastaDesde :: Int -> Int -> Int
mayorFibonacciHastaDesde i n | (fibonacci i) > n = fibonacci (i-1)
                             | otherwise = mayorFibonacciHastaDesde (i+1) n
--9
esFibonacci :: Int -> Bool
esFibonacci n = (n == (mayorFibonacciHasta n))

----------------------------------------------------------------------
menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise      = menorDivisorDesde n (k+1)

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n                      

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n = n
                   | otherwise = minimoPrimoDesde (n+1)
--10                   
esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n | n < 2 = False
                        | otherwise = n == sumaInicialDePrimosHasta 1 n 0

sumaInicialDePrimosHasta :: Int -> Int -> Int -> Int
sumaInicialDePrimosHasta i n t | n == 2 = 2
                               | t + nEsimoPrimo i > n = t
                               | otherwise = sumaInicialDePrimosHasta (i+1) (n) (t+nEsimoPrimo(i))
--11
tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 = maxSumaDivisores n1 n2 n1

maxSumaDivisores :: Int -> Int -> Int -> Int
maxSumaDivisores n1 n2 m | n1 > n2 = m
                         | sumaDivisores 1 n1 > sumaDivisores 1 m = maxSumaDivisores (n1+1) n2 n1
                         | otherwise = maxSumaDivisores (n1+1) n2 m

--12
tomaValorMin :: Int -> Int -> Int
tomaValorMin n1 n2 = minSumaDivisores n1 n2 n1

minSumaDivisores :: Int -> Int -> Int -> Int
minSumaDivisores n1 n2 m | n1 > n2 = m
                         | sumaDivisores 1 n1 < sumaDivisores 1 m = minSumaDivisores (n1+1) n2 n1
                         | otherwise = minSumaDivisores (n1+1) n2 m

sumaDivisores :: Int -> Int -> Int
sumaDivisores i n | i == n = i
                  | mod n i == 0 = i + sumaDivisores (i+1) n
                  | otherwise = sumaDivisores (i+1) n

--13
primosGem :: Int -> Int
primosGem n = primosGemDesde 2 n

primosGemDesde :: Int -> Int -> Int
primosGemDesde i n | (i + 2) > n = 0
                   | esPrimo i && esPrimo (i+2) = 1 + primosGemDesde (i+1) n
                   | otherwise = primosGemDesde (i+1) n

--14
proxPrimGem :: Int -> (Int, Int)
proxPrimGem n | esPrimo (n+1) && esPrimo (n+3) = (n+1, n+3)
              | otherwise = proxPrimGem (n+1)

----------------------------------------------------------------------
--15 a
largoSecuencia :: Int -> Int
largoSecuencia n = largoSecuenciaContar 0 n

largoSecuenciaContar :: Int -> Int -> Int
largoSecuenciaContar i 1 = i
largoSecuenciaContar i n = largoSecuenciaContar (i+1) (f1(n))

f1 :: Int -> Int
f1 n | mod n 2 == 0 = div n 2
     | otherwise = 3*n + 1

----------------------------------------------------------------------
--15 b
maxLargoSecuencia :: Int -> Int -> Int
maxLargoSecuencia n m | n == 1 = m
                      | largoSecuencia n > largoSecuencia m = maxLargoSecuencia (n-1) n
                      | otherwise = maxLargoSecuencia (n-1) m


