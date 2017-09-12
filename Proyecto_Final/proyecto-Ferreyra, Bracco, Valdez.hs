-------------------------------------------------------------------------------
----------------------------- DEMOSTRADOR BOOLEANO ----------------------------
-------------------------------------------------------------------------------

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
tablaverdad n = addtruefalse (tablaverdad (n-1))

-- Funcion auxiliar de "tablaverdad", que agrega "True" o "False" a cada
-- lista de booleanos:
addtruefalse :: [[Bool]] -> [[Bool]]
addtruefalse [] = []
addtruefalse (x:xs) = (x ++ [True]) : (x ++ [False]) : (addtruefalse xs)


-- EVALUA UNA FUNCION DADA CON LOS VALORES BOOLEANOS DADOS PARA CADA VARIABLE,
-- Y DA COMO RESULTADO "True" O "False" SEGUN CORRESPONDA:
check :: Form -> [Bool] -> Bool

check (P n) [] = error "Esto no es posible"
check (P n) (x:xs) | (n <= (length (x:xs))) && (n >= 1) = (x:xs) !! (n-1)
                   | otherwise = error "Esto no es posible"

check (Neg h) [] = error "Esto no es posible"
check (Neg h) (x:xs) | check h (x:xs) = False
                     | check h (x:xs) == False = True

check (And t y) [] = error "Esto no es posible"
check (And t y) (x:xs) = (check t (x:xs)) && (check y (x:xs))

check (Or a b) [] = error "Esto no es posible"
check (Or a b) (x:xs) = (check a (x:xs)) || (check b (x:xs))

check (Impl a b) [] = error "Esto no es posible"
check (Impl a b) (x:xs) | (check a (x:xs)) && ((check b (x:xs)) == False) = False
                        | otherwise = True

check (Equiv a b) [] = error "Esto no es posible"
check (Equiv a b) (x:xs) | (check a (x:xs)) == (check b (x:xs)) = True
                         | otherwise = False

check (Discre a b) [] = error "Esto no es posible"
check (Discre a b) (x:xs) | (check a (x:xs)) /= (check b (x:xs)) = True
                          | otherwise = False


-- DICE CUAL ES LA MAYOR VARIABLE DE UNA FORMULA:
mayorvar :: Form -> Int
mayorvar (P n) = n
mayorvar (Neg f) = mayorvar f
mayorvar (And f g) = max (mayorvar f) (mayorvar g)
mayorvar (Or f g) = max (mayorvar f) (mayorvar g)
mayorvar (Impl f g) = max (mayorvar f) (mayorvar g)
mayorvar (Equiv f g) = max (mayorvar f) (mayorvar g)
mayorvar (Discre f g) = max (mayorvar f) (mayorvar g)



-- PRUEBA LA SATISFACIBILIDAD DE UNA FORMULA:
esSatisfacible :: Form -> Bool
esSatisfacible a | checksatisfacible a (tablaverdad (mayorvar a)) = True
                 | otherwise = False

-- Funcion auxiliar de "esSatisfacible", que evalua la funcion en cada
-- lista de booleanos de la tabla de verdad:
checksatisfacible :: Form -> [[Bool]] -> Bool
checksatisfacible a [] = False
checksatisfacible a (x:xs) | check a x == False = checksatisfacible a xs
                           | otherwise = True



-- PRUEBA LA VALIDEZ DE UNA FORMULA:
esValida :: Form -> Bool
esValida a | checkvalida a (tablaverdad (mayorvar a)) = True
           | otherwise = False

-- Funcion auxiliar de "esValida", que evalua la funcion en cada lista de
-- booleanos de la tabla de verdad:
checkvalida :: Form -> [[Bool]] -> Bool
checkvalida a [] = True
checkvalida a (x:xs) | check a x = checkvalida a xs
                     | otherwise = False



-- EVALUA UNA FORMULA LOGICA Y LA CLASIFICA EN "SATISFACIBLE Y VALIDA",
-- "SATISFACIBLE PERO NO VALIDA" O "NO ES VALIDA" SEGUN CORRESPONDA:
evalua :: Form -> IO()
evalua a | (esSatisfacible a) && (esValida a) = print "Es Satisfacible y Valida"
         | (esSatisfacible a) && ((esValida a) == False) = print "Es Satisfacible pero no Valida"
         | ((esSatisfacible a) == False) && ((esValida a) == False) = print "No es Satisfacible"
