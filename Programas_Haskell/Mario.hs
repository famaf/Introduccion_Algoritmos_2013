-- FUNCIONES:

-- EJERCICIO 3: CALCULA EL PROMEDIO ENTRE DOS NUMEROS.
promedio :: Fractional a => a -> a -> a
promedio x y = (x + y) / 2


-- EJEMPLO DEL EJERCICIO 5 DEL PRACTICO 2: DADO UN ENTERO RETORNA SU SIGNO,
-- DE LA SIGUIENTE FORMA:
-- 1 SI x ES POSITIVO, -1 SI ES NEGATIVO Y 0 EN CUALQUIER OTRO.
sgn :: Int -> Int 
sgn x | 0 < x  = 1
      | x < 0  = -1
      | x == 0 = 0


-- EJERCICIO 5 A: QUE DADO UN ENTERO DEVUELVE True SI EL ENTERO SE ENCUENTRA
-- ENTRE 0 Y 9. 

entre0y9 :: Int -> Bool
entre0y9 x | 0 <= x && x <= 9 = True  -- entre0y9 x = 0 <= x && x <= 9
           | x <  0 || x > 9  = False

-- OTRA MANERA:
entre0y9XD :: Int -> Bool
entre0y9XD x | 0 <= x && x <= 9 = True
             | otherwise  = False


-- EJERCICIO 5 B: QUE DADA UNA TERNA DE ENTEROS DEVUELVE SU SEGUNDO ELEMENTO.
segundo3 :: (Int, Int, Int) -> Int
segundo3 (x, y, z) = (y)


-- EJERCICIO 5 C: QUE DADOS DOS ENTEROS LOS ORDENA DE MENOR A MAYOR.
ordena :: (Int, Int) -> (Int, Int)
ordena (x, y) = (min x y, max x y)


-- EJERCICIO 5 D: QUE DADO UN NUMERO QUE REPRESENTA EL PRECIO DE UNA COMPUTADORA,
-- RETORNE "Muy barato" SI EL PRECIO ES MENOR A 2000, "Demasiado caro" SI EL
-- PRECIO ES MAYOR A 5000, "Hay que verlo bien" SI EL PRECIO ESTA ENTRE 2000 Y
-- 5000, Y "Esto no puede ser" SI EL PRECIO ES NEGATIVO.
rangoprecio :: Int -> String
rangoprecio x | x < 2000 && x >= 0  = "Muy barato"
              | x > 5000 = "Demasiado caro"
              | x >= 2000 && x <= 5000 = "Hay que verlo bien"
              | x < 0 = "Esto no puede ser"


-- EJERCICIO 5 E: QUE DADO UN ENTERO RETORNE SU VALOR ABSOLUTO.
absoluto :: Int -> Int
absoluto x | x >= 0 = x
           | x < 0 = -x


-- EJERCICIO 5 F: QUE DADO UN ENTERO n DEVUELVE True  SI n ES MULTIPLO DE 2.
esmultiplo2 :: Int -> Bool
esmultiplo2 n = mod n 2 == 0

-- OTRA MANERA:
esmultiplo2XD :: Int -> Bool
esmultiplo2XD n | mod n 2 == 0 = True
                | otherwise = False


-- EJERCICIO 5 G: QUE DADO UN NUMERO x, QUE REPRESENTA EL PRECIO DEL PRODUCTO, Y UN
-- PAR (menor, mayor) QUE REPRESENTE EL RANGO DE PRECIOS QUE UNO ESPERA ENCONTRAR,
-- RETORNE "Muy barato" SI x ESTA POR DEBAJO DEL RANGO, "Demasiado caro" SI ESTA
-- POR ARRIBA DEL RANGO, "Hay que verlo bien" SI EL PRECIO ESTA EN EL RANGO, Y
-- "Esto no puede ser!" SI x ES NEGATIVO.
rpp :: Int -> (Int, Int) -> String
rpp x (y, z) | x < y && x > 0 = "Muy barato"
             | x > z = "Muy caro"
             | x >= y && x <= z = "Hay que verlo bien"
             | x < 0 = "Esto no puede ser!"


-- EJERCICIO 5 H: QUE DADA UNA TERNA DE ENTEROS DEVUELVE UNA TERNA DE VALORES
-- BOOLEANOS QUE INDICA SI CADA UNO DE LOS ENTEROS ES MAYOR QUE 3.
mayor3 :: (Int, Int, Int) -> (Bool, Bool, Bool)
mayor3 (x, y, z) = (x > 3, y > 3, z > 3)




--FUNCIONES RECURSIVAS:

-- EJERCICIO 4 A: QUE DADA UNA LISTA DE ENTEROS xs DEVUELVE UNA LISTA SOLO CON
-- LOS NUMEROS PARES CONTENIDOS EN xs, EN EL MISMO ORDEN Y CON LAS MISMAS
-- REPETICIONES (SI LAS HUBIERA).
solopares :: [Int] -> [Int]
solopares [] = []
solopares (x:xs) | (mod x 2 == 0) = x : solopares xs
                 | otherwise = solopares xs


-- EJERCICIO 4 B: QUE DADA UNA LISTA DE ENTEROS xs DEVUELVE UNA LISTA SOLO CON
-- LOS NUMEROS MAYORES QUE 10 CONTENIDOS EN xs.
mayoresque10 :: [Int] -> [Int]
mayoresque10 [] = []
mayoresque10 (x:xs) | (x > 10) = x:mayoresque10 xs
                    | otherwise = mayoresque10 xs


-- EJERCICIO 4 C: QUE DADO UN ENTERO n Y UNA LISTA DE ENTEROS xs DEVUELVE UNA
-- LISTA SOLO CON LOS NUMEROS MAYORES QUE n CONTENIDOS EN xs.
mayoresque :: Int -> [Int] -> [Int]
mayoresque n [] = []
mayoresque n (x:xs) | (x > n) = x:mayoresque n xs
                    | otherwise = mayoresque n xs


-- EJERCICIO 5 A: QUE DADA UNA LISTA DE ENTEROS DUPLICA CADA UNO DE SUS ELEMENTOS.
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (2 * x) : duplica xs


-- EJERCICIO 5 B: QUE DADO UN NUMERO n Y UNA LISTA, MULTIPLICA CADA UNO DE SUS
-- ELEMENTOS POR n.
multiplica :: Int -> [Int] -> [Int]
multiplica n [] = []
multiplica n (x:xs) = map (*n) (x:xs)


-- EJERCICIO 6 A: QUE DADA UNA LISTA DEVUELVE True SI ÉSTA CONSISTE SOLO DE
-- NUMEROS MENORES QUE 10. 
todosmenores10 :: [Int] -> Bool
todosmenores10 [] = True
todosmenores10 (x:xs) | x < 10 = todosmenores10 xs
                      | otherwise  = False


-- EJERCICIO 6 B: QUE DADA UNA LISTA DECIDE SI EXISTE ALGUN 0 EN ELLA.
hay0 :: [Int] -> Bool
hay0 [] = False
hay0 (x:xs) | x /= 0 = hay0 xs
            | x == 0 = True


-- EJERCICIO 6 C: QUE DADA UNA LISTA DEVUELVE LA SUMA DE TODOS SUS ELEMENTOS.
suml :: [Int] -> Int
suml [] = 0
suml (x:xs) = x + suml xs



--FUNCIONES GENERALES:

-- EJERCICIO 8 A: QUE CALCULA EL MAXIMO ELEMENTO DE UNA LISTA DE ENTEROS.
max1 :: [Int] -> Int
max1 [] = 0
max1 (x:xs) = (max x (max1 xs))


-- OTRA MANERA:
max2 :: [Int] -> Int
max2 [] = error "La lista no tiene ningun elemento"
max2 (x:xs) = maximum (x:xs)


-- EJERCICIO 8 B: QUE DADA UNA LSITA DE PARES DE NUMEROS, DEVUELVE LA
-- SUMATORIA DE TODOS LOS ELEMENTOS DE LA LISTA.
sumpares :: [(Int,Int)] -> Int
sumpares [] = 0
sumpares ((x,y):xs) = (x+y) + sumpares xs


-- EJERCICIO 8 C: QUE DADA UNA LISTA DEVUELVE True SI ESTA CONSISTE SOLO DE 0s y 1s.
todos0y1 :: [Int] -> Bool
todos0y1 [] = True
todos0y1 (x:xs) | x == 1 || x == 0 = todos0y1 xs
                | otherwise = False


-- todos0y11 :: [Int] -> Bool
-- todos0y11 [] = True
-- todos0y11 (x:xs) | x == 1 || x == 0 = todos0y1 xs


-- EJERCICIO 8 D: QUE DADA UNA LISTA DE ENTEROS DEVUELVE LA LISTA PERO QUITANDO
-- TODOS LOS 0s.
quitar0 :: [Int] -> [Int]
quitar0 [] = []
quitar0 (x:xs) | x /= 0 = x:quitar0 xs
               | x == 0 = quitar0 xs


-- EJERCICIO 8 E: QUE DEVUELVE EL ULTIMO ELEMENTO DE LA LISTA.
ultimo :: [a] -> a
ultimo (x:xs) = last (x:xs)

-- ultimo :: [a] -> a
-- ultimo (x:xs) = head (reverse (x:xs)) 


--EJERCICIO 8 F:
-- COMPARA DOS LISTAS DE ENTEROS: DETERMINA SI DOS LISTAS SON IGUALES, ES DECIR
-- SI CONTIENEN LOS MISMOS ELEMENTOS EN LAS MISMAS POSICIONES RESPECTIVAMENTE.
iguales :: [Int] -> [Int] -> Bool
iguales [] [] = True
iguales (x:xs) [] = False
iguales [] (x:xs) = False
iguales (x:xs) (y:ys) | x==y = iguales xs ys
                      | x/=y = False


-- COMPARA DOS LISTAS DE STRING: DETERMINA SI DOS LISTAS SON IGUALES, ES DECIR
-- SI CONTIENEN LOS MISMOS ELEMENTOS EN LAS MISMAS POSICIONES RESPECTIVAMENTE.
iguales2 :: [String] -> [String] -> Bool
iguales2 [] [] = True
iguales2 (x:xs) [] = False
iguales2 [] (x:xs) = False
iguales2 (x:xs) (y:ys) | x==y = iguales2 xs ys
                       | x/=y = False


-- COMPARA DOS LISTAS CUALES QUIERA SEA:
iguales3 :: Eq a => [a] -> [a] -> Bool
iguales3 [] [] = True
iguales3 (x:xs) [] = False
iguales3 [] (x:xs) = False
iguales3 (x:xs) (y:ys) | x==y = iguales3 xs ys
                       | x/=y = False
