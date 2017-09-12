-- Da todas las combinaciones sin importar el orden de una cantidad x de
-- variables (en este caso serian los vertices las variables y el resultado
-- las aristas si el grafo fuera completo)
combinaciones :: [Char] -> [(Char, Char)]
combinaciones [] = []
combinaciones (x:xs) = f x xs ++ combinaciones xs


f :: Char -> [Char] -> [(Char, Char)]
f y [] = []
f y (x:xs) = (y, x) : f y xs


u :: [(Char, Char)] -> [(Char, Char)] -> Int -> [(Char, Char)]
u [] ys z = ys
u xs [] z = []
u xs (y:ys) z | xs !! z == y = u xs ys z
              | otherwise = y : u xs ys z

l :: [(Char, Char)] -> [(Char, Char)] -> Int -> [(Char, Char)]
l xs ys 0 = u xs ys 0
l xs ys z = l xs (u xs ys z) (z-1)

-- FUNCION FINAL Q DADO LOS VERTICES Y LA LISTA DE ARISTAS Q COMPONEN EL GRAFO
-- DA EL COMPLEMENTO!
complemento :: [Char] -> [(Char, Char)] -> [(Char, Char)]
complemento fg aristas = l aristas (combinaciones fg) ((length aristas) - 1)

-- Compara el primer elemento de la primer lista y lo da si esta en la segunda
-- lista. de lo contrario da ('a', 'a')
dejalosqestan :: [(Char, Char)] -> [(Char, Char)] -> (Char, Char)
dejalosqestan (x:xs) [] = ('a', 'a')
dejalosqestan (x:xs) (y:ys) | x == y = x
                            | otherwise = dejalosqestan (x:xs) ys



-- Compara el primer elemento de la primer lista y da todos los de la segunda
-- que no son iguales primer elemento de la primera
losqnoestan :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
losqnoestan (x:xs) [] = []
losqnoestan (x:xs) (y:ys) | x == y = ('a', 'a') : losqnoestan (x:xs) ys
                          | otherwise = y: losqnoestan (x:xs) ys

-- Quita todos los ('a', 'a') de la lista
g :: [(Char, Char)] -> [(Char, Char)]
g [] = []
g (y:ys) | y == ('a', 'a') = g ys
         |otherwise = y : g ys

-- Compara cada elemento de la primer lista y los deja si estan en la segunda
construir :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
construir [] (y:ys) = []
construir (x:xs) (y:ys) = dejalosqestan [x] (y:ys) : construir xs (y:ys)

-- Compara cada elemento de la primer lista y no los deja si estan en la segunda
construir2 :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
construir2 [] (y:ys) = []
construir2 (x:xs) [] = []
construir2 (x:xs) (y:ys) = losqnoestan [x] (y:ys) ++ construir2 (losqnoestan [x] (y:ys)) (x:ys)

{-
[1,2] [3,1,2]
[3,2] ++ quitarrepetidos [2] [3,2]
[3,2] ++ [3] ++ quitarrepetidos [] [3]
-}

-- quitarrepetidos :: [Int] -> [Int] -> [Int]
-- quitarrepetidos [] (y:ys) = []
-- quitarrepetidos (x:xs) (y:ys) = funcion (x:xs) (y:ys) ++ funcion xs (y:ys)


-- funcion :: [Int] -> [Int] -> [Int] -> [Int]
-- funcion  [] [] [] = []
-- funcion [] (y:ys) (h:hs) = []
-- funcion (x:xs) [] (h:hs) = funcion xs hs hs
-- funcion (x:xs) (y:ys) (h:hs) | x== y = funcion (x:xs) ys (h:hs)
--                              |otherwise = y : funcion (x:xs) ys (h:hs)



funcion :: [Int] -> [Int] -> [Int] -> [Int]
funcion  (x:xs) [] (h:hs) = funcion xs (funcion (x:xs) (h:hs) (h:hs)) (funcion (x:xs) (h:hs) (h:hs))
funcion [] (y:ys) (h:hs) = (y:ys)
funcion (x:xs) (y:ys) (h:hs) | x == y = funcion (x:xs) ys (h:hs)
                             | otherwise = y : funcion (x:xs) ys (h:hs)

{-
funcion [1,5] [1,6] [1,6]
funcion [1,5] [6] [1,6]
6 : funcion [1,5] [] [1,6]
6 : funcion [5] [6] [6]
6 : funcion [5] [6] [6]
6 : 6 : funcion [5] [] [6]
6 : 6 : funcion [] [] []
-}


{-
funcion [1,2,6] [1,2,6,8] [1,2,6,8]
funcion [1,2,6] [2,6,8] [1,2,6,8]
2 : funcion [1,2,6] [6,8] [1,2,6,8]
2 : 6 : funcion [1,2,6] [8] [1,2,6,8]
2 : 6 : 8 : funcion [1,2,6] [] [1,2,6,8]
2 : 6 : 8 : funcion [1,2,6] [] [1,2,6,8]
funcion [2,6] [2,6,8] [2,6,8]
funcion [2,6] [6,8] [2,6,8]
6 : funcion [2,6] [8] [2,6,8]
6 : 8 : funcion [2,6] [] [2,6,8]
funcion [6] [6,8] [6,8]
funcion [6] [8] [6,8]
8 : funcion [6] [] [6,8]
funcion [] [8] [8]
-}


-- Resta de cojuntos Int
j :: [Int] -> [Int] -> Int -> [Int]
j [] ys z = ys
j xs [] z = []
j xs (y:ys) z | xs !! z == y = j xs ys z
              |otherwise = y : j xs ys z

m :: [Int] -> [Int] -> Int -> [Int]
m xs ys 0 = j xs ys 0
m xs ys z = m xs (j xs ys z) (z-1)
-- siiiiiiiiiiiiiiii
