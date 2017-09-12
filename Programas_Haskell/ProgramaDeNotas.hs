notas :: [Int] -> [Int]
notas [] = []
notas (x:xs) | x >= 2 = x : notas xs
             | x < 2 = notas xs


notas1 :: [(Int, Int)] -> [(Int, Int)]
notas1 [] = []
notas1 ((x,y):xs) | x >= 4 && y >= 4 = (x,y): notas1 xs
                  | x < 4 && y < 4 = notas1 xs


notas2 :: [(String, Int)] -> [(String, Int)]
notas2 [] = []
notas2 ((x,y):xs) | y >= 4 = (x,y): notas2 xs
                  | y < 4 = notas2 xs

notas3 :: [(String, Int)] -> [(String, Int)]
notas3 [] = []
notas3 ((x,y):xs) | y >= 7 = (x,y): notas3 xs
                  | y < 7 = notas3 xs

notas4 :: (String, Int) -> String
notas4 (x, y) | y >= 7 = "Promocionado"
              | y >= 4 && y < 7 = "Regular"
              | y < 4 && y > 0 = "Desaprobado"


todo :: [(String,Int)] -> ([String], [String], [String])
todo [] = ([],[],[]) 
todo ((x,y):xs) | y >= 7 = (x:ps, rs, ds)
                | y >= 4 = (ps, x:rs, ds)
                | y < 4 = (ps, rs ,x:ds)

                where (ps, rs, ds) = todo xs


-- DADO UN NOMBRE Y DOS NOTAS DEVUELVE EL NOMBRE Y EL PROMEDIO DE LAS DOS NOTAS:
-- "Fractional a : indica que todos los que tengan "a" van a tener tipo fraccional".
-- " "=>" : Bolsa de tipos"
promo :: Fractional a => [(String, a, a)] -> [(String, a)]
promo [] = []
promo ((x, y, z):xs) = (x, ((y + z) / 2)) : promo xs


-- DADO UN NOMBRE Y DOS NOTAS DEVUELVE EL NOMBRE Y EL PROMEDIO DE LAS DOS NOTAS:
-- "Float : esto quiere decir que es un numero de coma flotante"
promo2 :: [(String, Float, Float)] -> [(String, Float)]
promo2 [] = []
promo2 ((x, y, z):xs) = (x, ((y + z) / 2)) : promo xs


-- FUNCION PROMEDIO: dada una lista de numeros devuelve su promedio
-- sum : funcion suma
-- fromIntegral : convierte al entero en un fraccional para poder realizar la division:
promedio :: Fractional a => [a] -> a
promedio xs = (sum xs)/ (fromIntegral (length xs))

-- toRational : lo expresa en haskell como una division:

promediofinal xs = (sum xs) / toRational (length xs)


-- QUE DADO UNA LISTA DE NOMBRES Y NUMEROS DEVUELVE LOS NOMBRES Y EL PROMEDIO
-- DE LOS NUMEROS:
promo3 :: [(String, [Float])] -> [(String, Float)]
promo3 [] = []
promo3 ((x,(ys)):xs) = (x,(promedio ys)):promo3 xs


-- QUE DADO UNA LISTA DE NOMBRES Y NOTAS DEVUELVE UNA LISTA DE NOMBRES Y NOTAS
-- CON SIN APLASOS (>=4):
promo4 :: [(String, [Float])] -> [(String, Float)]
promo4 [] = []
promo4 ((x,(ys)):xs) = (x,(promedio (filter (4<=) ys))):promo4 xs
