convertir :: Int -> Int -> Int
convertir n b = convertir2 n b 0

convertir2 :: Int -> Int -> Int -> Int
convertir2 0 b c = 0
convertir2 n b c = (mod n 10) * b^c + convertir2 (div n 10) b (c + 1)
