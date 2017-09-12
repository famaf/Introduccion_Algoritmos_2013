data Form = P Int
          | Neg Form
          | And Form Form
          | Or Form Form
          | Impl Form Form
          | Equiv Form Form
          | Discre Form Form


-- DA LA TABLA DE VERDAD DE "n" CANTIDAD DE VARIABLES:
tablaverdad :: Int -> [[Bool]]
tablaverdad 1 = [[True], [False]]
tablaverdad n = auxiliar (tablaverdad (n-1))

auxiliar :: [[Bool]] -> [[Bool]]
auxiliar [] = []
auxiliar (x:xs) = (x ++ [True]) : (x ++ [False]) : (auxiliar xs)


-- DADA UNA FORMULA LOGICA Y UNA LISTA DE BOOLEANOS DA "True" O "False",
-- SEGUN CORRESPONDA TENIENDO EN CUENTA LA OPERACION LOGICA IMPUESTA A LAS
-- VARIABLES:
check :: Form -> [Bool] -> Bool

check (P n) [] = error "Esto no es posible"
check (P n) (x:xs) | (n <= (length (x:xs))) && (n >= 1) = (x:xs) !! (n-1)
                   | otherwise = error "Esto no es posible"

check (Neg h) [] = error "Esto no es posible"
check (Neg h) (x:xs) | check h (x:xs) == True = False
                     | check h (x:xs) == False = True

check (And t y) [] = error "Esto no es posible"
check (And t y) (x:xs) = (check t (x:xs)) && (check y (x:xs))

check (Or a b) [] = error "Esto no es posible"
check (Or a b) (x:xs) = (check a (x:xs)) || (check b (x:xs))

check (Impl a b) [] = error "Esto no es posible"
check (Impl a b) (x:xs) | ((check a (x:xs)) == True) && ((check b (x:xs)) == False) = False
                        | otherwise = True

check (Equiv a b) [] = error "Esto no es posible"
check (Equiv a b) (x:xs) | (check a (x:xs)) == (check b (x:xs)) = True
                         | otherwise = False

check (Discre a b) [] = error "Esto no es posible"
check (Discre a b) (x:xs) | (check a (x:xs)) /= (check b (x:xs)) = True
                          | otherwise = False


-- DICE CUALES ES LA VARIABLE MAS GRANDE DE UNA FORMULA:
mayorvar :: Form -> Int
mayorvar (P n) = n
mayorvar (Neg f) = mayorvar f
mayorvar (And f g) = max (mayorvar f) (mayorvar g)
mayorvar (Or f g) = max (mayorvar f) (mayorvar g)
mayorvar (Impl f g) = max (mayorvar f) (mayorvar g)
mayorvar (Equiv f g) = max (mayorvar f) (mayorvar g)
mayorvar (Discre f g) = max (mayorvar f) (mayorvar g)


-- DICE SI UNA FORMULA ES SATISFACTIBLE O NO ES SATISFACTIBLE:
esSatisfactible :: Form -> Bool
esSatisfactible a | auxiliar2 a (tablaverdad (mayorvar a)) = True
                  | otherwise = False

auxiliar2 :: Form -> [[Bool]] -> Bool
auxiliar2 a [] = False
auxiliar2 a (x:xs) | check a x == False = auxiliar2 a xs
                   | otherwise = True


-- DICE SI UNA FORMULA ES VALIDA O NO ES VALIDA:
esValida :: Form -> Bool
esValida a | auxiliar3 a (tablaverdad (mayorvar a)) = True
           | otherwise = False

auxiliar3 :: Form -> [[Bool]] -> Bool
auxiliar3 a [] = True
auxiliar3 a (x:xs) | check a x == True = auxiliar3 a xs
                   | otherwise = False


-- EVALUA UNA FORMULA LOGICA Y DICE SI ES "SATISFACTIBLE Y VALIDA",
-- "SATISFACTIBLE PERO NO VALIDA" O "NO ES VALIDA":
evalua :: Form -> IO()
evalua a | ((esSatisfactible a) == True) && ((esValida a) == True) = print "Es Satisfacible y Valida"
         | ((esSatisfactible a) == True) && ((esValida a) == False) = print "Es Satisfacible pero no Valida"
         | ((esSatisfactible a) == False) && ((esValida a) == False) = print "No es Satisfacible"
