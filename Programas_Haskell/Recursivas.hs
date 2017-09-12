-- FUNCION RECURSIVA "map":
mapa :: (a -> b) -> [a] -> [b]
mapa f [] = []
mapa f (x:xs) = map (f) (x:xs)


-- FUNCION RECURSIVA "filter":
filtro :: (a -> Bool) -> [a] -> [a]
filtro p [] = []
filtro p (x:xs) | p x = x:filtro p xs
                | not (p x) = filtro p xs


-- FUNCION RECURSIVA "foldl":
foldearl:: (a -> b -> a) -> a -> [b] -> a
foldearl f l [] = l
foldearl f l (x:xs) = foldearl f (f l x) xs
