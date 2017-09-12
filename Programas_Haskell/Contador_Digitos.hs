-- Cuenta los digitos de un numero
cont :: Int -> Int
cont 0 = 0
cont n = 1 + cont (div (abs n) 10)


-- Cuenta los digitos de un numero (convirtiendo el numero en una lista de
-- caracteres y luego sacando su cardinal):
cont2 :: Int -> Int
cont2 0 = 0
cont2 n = length (show n)

-- "show" = muestra un numero como una string
-- Ejemplo: show 45 = "45"


-- Demostracion con un ejemplo de como funciona:

{-
cont 158646
= {definicion de cont y div}
1 + cont 15864
= {definicion de cont y div}
1 + 1 + cont 1586
= {definicion de cont y div}
1 + 1 + 1 + cont 158
= {definicion de cont y div}
1 + 1 + 1 + 1 + cont 15
= {definicion de cont y div}
1 + 1 + 1 + 1 + 1 + cont 1
= {definicion de cont y div}
1 + 1 + 1 + 1 + 1 + 1 + cont 0
= {definicion de cont}
1 + 1 + 1 + 1 + 1 + 1 + 0
= {aritmetica}
6
-}
