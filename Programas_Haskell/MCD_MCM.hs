-- CALCULA EL MAXIMO COMUN DIVISOR (M.C.D.) DE DOS NUMEROS:
mcd :: Int -> Int -> Int
mcd 0 y = abs y
mcd x y = mcd (mod y x) x


-- CALCULA EL MINIMO COMUN MULTIPLO (M.C.M.) DE DOS NUMEROS:
mcm :: Int -> Int -> Int
mcm x y = div (abs (x * y)) (mcd x y)
