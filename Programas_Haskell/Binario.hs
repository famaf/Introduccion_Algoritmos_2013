-- Convierte un numero de decimal a binario
binario :: Int -> String
binario 0 = []
binario x | mod x 2 == 0 = binario (div x 2) ++ ['0']
          | otherwise = binario (div x 2) ++ ['1']

-- Muestra numeros en binario hasta en numero x (en decimal)
binariohasta :: Int -> [String]
binariohasta 0 = []
binariohasta x = binariohasta (x - 1) ++ [binario x]

-- Donde "x" es el numero a convertir e "y" es la base en el rango [2, 9]
cambiobase :: Int -> Int -> String
cambiobase 0 y = []
cambiobase x y = (cambiobase (div x y) y) ++ [hacerchar (mod x y)]

-- Muestra numeros en base y hasta el numero x
enbasehasta :: Int -> Int -> [String]
enbasehasta 0 y = []
enbasehasta x y = enbasehasta (x-1) y ++ [cambiobase x y]

-- Convierte un entero de 0-9 en un char
hacerchar :: Int -> Char
hacerchar x = head (show x)

-- Transformar hasta hexadecimal
hastahexa :: Int -> Int -> String
hastahexa 0 y = []
hastahexa x y | y <= 9 = (hastahexa (div x y) y) ++ [hacerchar (mod x y)]
              | otherwise = "completar"

debinarioadecimal :: String -> Int -> Int
debinarioadecimal x y | binario y == x = y
                      | otherwise = debinarioadecimal x (y+1)
