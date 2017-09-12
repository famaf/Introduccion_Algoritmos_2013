repetir :: Int -> String
repetir 0 = ""
repetir n = "=" ++ repetir (n-1)

pene :: Int -> String
pene 0 = "8D"
pene n | n >= 0 = "8" ++ repetir n ++ "D"
       | otherwise = error "Esto no es humano!!!"
