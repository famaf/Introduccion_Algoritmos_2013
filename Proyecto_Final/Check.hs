data Form = P Int
          | Neg Form
          | And Form Form
          | Or Form Form
          | Impl Form Form
          | Equiv Form Form


-- DADA UNA FORMULA LOGICA Y UNA LISTA DE BOOLEANOS DA "True" O "False",
-- SEGUN CORRESPONDA TENIENDO EN CUENTA LA OPERACION LOGICA IMPUESTA A LAS VARIABLES:
check :: Form -> [Bool] -> Bool

check (P n) [] = error "Esto no es posible"
check (P 1) (x:xs) = x
check (P n) (x:xs) = check (P (n-1)) xs

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

{-
EJEMPLO DE SU USO:

check (P 4) [True, False, False, True]
=
check (P 3) [False, False, True]
=
check (P 2) [False, True]
=
check (P 1) [True]
=
True
-}
