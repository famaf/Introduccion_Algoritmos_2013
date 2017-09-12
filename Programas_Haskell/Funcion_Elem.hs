-- DADO UN ELEMENTO Y UNA LISTA DE ELEMENTOS, DA UN VALOR DE VERDAD SI ESE
-- ELEMENTO SE ENCUENTRA EN LA LISTA INGRESADA:
elemento :: Eq a => a -> [a] -> Bool
elemento x [] = False
elemento x (y:ys) = (y == x) || (elemento x ys)
