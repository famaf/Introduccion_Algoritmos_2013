-- CALCULA LAS RAICES DE UNA ECUACION CUADRATICA O DE GRADO 2:
raices (a,b,c) = ((-b + sqrt (b**2 - 4*a*c)) / 2*a, (-b - sqrt (b**2 - 4*a*c)) / 2*a)


-- CALCULA EL VETICE SOBRE EL EJE DE LAS X:
verticeX (a,b,c) = (-b/2*a)


-- CALCULA EL VETICE SOBRE EL EJE DE LAS Y:
verticeY (a,b,c) = (a*(-b/2*a)**2+b*(-b/2*a)+c)


-- CALCULA LA DISTANCIA ENTRE DOS PUNTOS DE LA RECTA REAL:
distancia :: Int -> Int -> Int
distancia x y | x >= y = x - y
              | x < y = - x + y


-- CALCULA LA DISTANCIA ENTRE DOS PUNTOS DE PLANO CARTESIANO:
distancia2 (a ,b) (c, d) = sqrt ((a - c)^2 + (b - d)^2)


-- CALCULA EL FACTORIAL DE UN NUMERO:
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- QUE DADO LOS VALORES DE UN POLINOMIO DE 2º GRADO
-- DIGA QUE TIPO SON SUS RAICES:
discriminante :: (Num a, Ord a) => a -> a -> a -> [Char]
discriminante a b c | discri > 0 = "Dos raices reales"
                    | discri == 0 = "Una raiz doble"
                    | discri < 0 = error "Raices Imaginarias"

                    where discri = b*b - 4 * a * c


-- QUE DADO UN String DEVUELVE ESE MISMO String PERO EXPANDIDO:
expandir :: String -> String
expandir [x] = [x]
expandir (x:xs) = x : ' ' : expandir xs


-- DEVUELVE LA LISTA PERO CON LOS ESPACIOS IMPARES.
filtro2 [] = []
filtro2 [x] = [x]
filtro2 (x:y:xs) = x : filtro2 xs


-- DADO UN NUMERO Y UNA LISTA DE NUMEROS, INSERTA ESE NUMERO EN LA LISTA EN SU
-- LUGAR CORRESPONDIENTE DE FORMA ORDENADA AL RESTO:
inserta :: Int -> [Int] -> [Int]
inserta x [] = [x]
inserta x (y:ys) | x < y = x : (y:ys)
                 | x >= y = y : (inserta x ys)

-- inserta 4 [1,2,3]
-- = {definicion de inserta}
-- 1 : (inserta 4 [2,3])
-- = {definicion de inserta}
-- 1 : 2 : (inserta 4 [3])
-- = {definicion de inserta}
-- 1 : 2 : 3 : (inserta 4 [])
-- = {definicion de inserta}
-- 1 : 2 : 3 : [4]
-- = {definicion de :}
-- [1,2,3,4]


-- ORDENA LOS ELEMENTOS DE UNA LISTA DESORDENADOS DE MAYOR A MENOR:
ordena :: [Int] -> [Int]
ordena [] = []
ordena (x:xs) = inserta x (ordena xs)

{-
ordena [1,45,4,2]
= {definicion de ordena}
inserta 1 (ordena [45,4,2])
= {definicion de inserta}
inserta 1 (inserta 45 (ordena [4,2]))
= {definicion de inserta}
inserta 1 (inserta 45 (inserta 4 (ordena [2])))
= {definicion de inserta}
inserta 1 (inserta 45 (inserta 4 (inserta 2 (ordena []))))
= {definicion de inserta}
inserta 1 (inserta 45 (inserta 4 (inserta 2 ([]))))
= {definicion de inserta}
1 : 2 : 4 : 45 : []
= {definicion de :}
[1,2,4,45]
-}


-- MUESTRA EL ALGORITMO DE DIVISION DE DOS NUMEROS ENTEROS:
dividir :: Int -> Int -> String
dividir 0 0 = error "Indeterminacion"
dividir n 0 = error "Infinito"
dividir n m = (show n) ++ " = " ++ (show m) ++ " * "  ++ show (div n m) ++ " + " ++ show (mod n m)


--CONVIERTE UN NUMERO DE CUALQUIER BASE (PREFERENTEMENTE =<9) A BASE 10:
convertir :: Int -> Int -> Int
convertir n b = convertir1 n b 0

convertir1:: Int -> Int -> Int -> Int
convertir1 0 b p = 0 
convertir1 n b p = (mod n 10)* b^p + convertir1 (div n 10) b (p+1)
