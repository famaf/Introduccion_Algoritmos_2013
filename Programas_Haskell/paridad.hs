paridad :: Int -> String
paridad n | mod n 2 == 0 = "ES UN NUMERO PAR"
          | otherwise = "ES UN NUMERO IMPAR"
