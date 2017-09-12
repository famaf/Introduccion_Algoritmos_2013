-- COMPARA DOS PALABRAS Y DICE SI SON IGUALES O NO:
comparar :: String -> String -> Bool
comparar [] [] = True
comparar [] (x:xs) = True
comparar (y:ys) [] = False
comparar (y:ys) (x:xs) | x == y = comparar ys xs
                       | otherwise = False


-- DADA UN PALABRA Y UNA LISTA DE PALABRAS,
-- DEVUELVE LA/S QUE MAS SE ASEMEJEN A LA PALABRA INGRESADA:
diccionario :: String -> [String] -> [String]
diccionario ys [] = []
diccionario ys (xs:xss) | comparar ys xs = xs : diccionario ys xss
                        | otherwise = diccionario ys xss


--DEVUELVE LA PRIMERA LETRA DE UNA PALABRA INGRESADA:
primeraletra :: String -> Char
primeraletra "" = error "no hay contenido"
primeraletra (x:xs) = x


-- DADA UNA PALABRA Y UNA LISTA DE PALABRAS DEVUELVE LA PALABRA MISMA DE LA
-- LISTA O "Mal" SI NO ESTA:
prueba :: String -> [String] -> String
prueba (x:xs) ((y:ys):yss) | x == y = (y:ys)
                           | x /= y = "Mal"


--DADA UNA PALABRA Y UNA LISTA DE PALABRAS, DEVUELVE LAS PALABRAS SI LAS
-- PRIMERAS LETRAS SON IGUALES (CONSERVANDO LA PALABRA):
prueba2 :: String -> [String] -> [String]
prueba2 (x:xs) [] = []
prueba2 (x:xs) ((y:ys):yss) | x == y = (y:ys) : prueba2 (x:xs) yss
                            | otherwise = prueba2 (x:xs) yss
