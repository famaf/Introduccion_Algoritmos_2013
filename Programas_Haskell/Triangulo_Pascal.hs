-- Dado n € N, la función nos regresa el n-simo renglón del
-- triángulo de Pascal.
pascal :: Int -> [Int]
pascal 1 = [1]
pascal 2 = [1, 1]
pascal n = [1] ++ (sumPares (pascal (n-1))) ++ [1] 

         where sumPares [x,y] = [x+y]
               sumPares (x:(y:ys)) = (x+y):(sumPares (y:ys))

{-
pascal 4
=
[1] ++ (sumPares (pascal 3)) ++ [1]
=
[1] ++ (sumPares (sumPares (pascal (2))) ++ [1]) ++ [1]
=
[1] ++ (((sumPares (sumPares [1,1]))) ++ [1]) ++ [1]
=
[1] ++ (sumPares [2] ++ [1]) ++ [1]
-}
