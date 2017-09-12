-- DA UNA LISTA DE LOS NUMEROS PRIMOS DE EL NUMERO INGRESADO HASTA EL NUMERO 2:
primos :: Int -> [Int]
primos 2 = [2]
primos x | esprimo (abs x) (abs x) = x : primos ((abs x)-1)
         | otherwise = primos ((abs x)-1)


-- DA UNA LISTA DE LOS NUMEROS PRIMOS HASTA EL NUMERO INGRESADO:
primosordenados n = revertir (primos n)


-- DICE SI UN NUMERO ES PRIMO (HAY QUE INGRESARLO DOS VECES SEGUIDAS AL NÚMERO)
-- DANDO "True" o "False":
esprimo :: Int -> Int -> Bool
esprimo n 0 = False
esprimo n 1 = False
esprimo n 2 = True
esprimo n x | mod n 2 == 0 = False
            | mod n ((abs x)-1) == 0 = False
            | otherwise = esprimo n ((abs x)-1)


-- DEVUELVE UNA LISTA PERO AL REVES:
revertir :: [Int] -> [Int]
revertir [] = []
revertir (x:xs) = (revertir xs) ++ [x]


-- DEVUELVE UNA LISTA DE LISTAS DE LOS ELEMENTOS DE UNA LISTA DADA:
enlistar ::[Int] -> [[Int]]
enlistar [] = []
enlistar (x:xs) = [x] : enlistar xs
