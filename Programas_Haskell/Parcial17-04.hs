-- QUE DADO UN PAR ORDENADO DE NUMEROS Y UN NUMERO, DICE SI EL NUMERO ES MAYOR
-- QUE CUALQUIER NUMERO DEL PAR ORDENADO:
algomayor :: (Int,Int) -> Int -> Bool
algomayor (x,y) n | x > n || n < y = True
                  | x < n || n > y = False


-- QUE DADA UN LISTA DE NUMEROS DEVUELVE SOLAMENTE LOS NUMEROS NEGATIVOS:
negativo :: [Int] -> [Int]
negativo [] = []
negativo (x:xs) | x > 0 = negativo xs
                | x < 0 = x : negativo xs


-- QUE DADA UNA LISTA DE NOTAS, DEVUELVE UNA LISTA CON LAS NOTAS Y UNA PALABRA,
-- "Libre" SI ES X<5 Y X>=0 Y "Regular" SI ES >=5 Y <=10. 
clasifica :: [Int] -> [(Int, String)]
clasifica [] = []
clasifica (x:xs) | x < 5 && x >= 0 = (x, "Libre") : clasifica xs
                 | x >= 5 && x <= 10 = (x, "Regular") : clasifica xs
                 | x < 0 || x > 10 = error "Esto no puede ser posible"
