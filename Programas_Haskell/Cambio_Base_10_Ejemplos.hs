convertir :: Int -> Int -> Int
convertir n b = convertir2 n b 0

convertir2 :: Int -> Int -> Int -> Int
convertir2 0 b c = 0
convertir2 n b c = (mod n 10) * b^c + convertir2 (div n 10) b (c + 1)

-- Ejemplo de uso:
--    Pasar (1100) base 2 a base 10

convertir 1100 2
= {DEFINICION DE CONVERTIR}
convertir2 1100 2 0
= {DEFINICION DE CONVERTIR2}
(mod 1100 10) * 2^0 + convertir2 (div 1100 10) 2 (0 + 1)
= {DEFINICION DE "MOD", "DIV", ARITMETICA Y CONVERTIR}
0 * 1 + convertir2 110 2 1
= {DEFINICION DE CONVERTIR2}
0 + (mod 110 10) * 2^1 + convertir2 (div 110 10) 2 (1 + 1)
= {DEFINICION DE "MOD", "DIV", ARITMETICA Y CONVERTIR}
0 + 0 * 2 + convertir2 11 2 2
= {DEFINICION DE CONVERTIR2}
0 + 0 + (mod 11 10) * 2^2 + convertir2 (div 11 10)  2 (2 + 1)
= {DEFINICION DE "MOD", "DIV", ARITMETICA Y CONVERTIR}
0 + 0 + 1 * 4 + convertir2 1 2 3
= {DEFINICION DE CONVERTIR2}
0 + 0 + 4 + (mod 1 10) * 2^3 + convertir2 (div 1 10) 2 (3 + 1)
= {DEFINICION DE "MOD", "DIV", ARITMETICA Y CONVERTIR}
0 + 0 + 4 + 1 * 8 + convertir 0 2 4
= {DEFINICION DE CONVERTIR2}
0 + 0 + 4 + 8 + 0
= {DEFINICION DE SUMA}
12

[] -- DEMOSTRADO
