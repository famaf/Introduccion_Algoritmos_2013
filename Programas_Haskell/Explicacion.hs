duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = map (*2) (x:xs)


-- EXPLICACION DE COMO TRABAJA HASKELL CON LA FUNCION "sumas":
sumas :: [Int] -> Int
sumas [] = 0
sumas (x:xs) = x + sumas xs

sumas [1,2,3,4,5]
= {deficion de sumas}
1 + sumas [2,3,4,5]
= {deficion de sumas}
1 + 2 + sumas [3,4,5]
= {deficion de sumas}
1 + 2 + 3 + sumas [4,5]
= {deficion de sumas}
1 + 2 + 3 + 4 + sumas [5]
= {deficion de sumas}
1 + 2 + 3 + 4 + 5 + sumas []
= {deficion de sumas}
1 + 2 + 3 + 4 + 5 + 0
= {aritmetica}
15


-- EXPLICACION DE COMO TRABAJA HASKELL CON LA FUNCION "duplica":
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = 2 * x : duplica xs

duplica [1,2,3,4,5]
= {definicion de duplica}
2 * 1 : duplica [2,3,4,5]
= {definicion de duplica}
2 : 2 * 2 : duplica [3,4,5]
= {definicion de duplica y *}
2 : 4 : 2 * 3 : duplica [4,5]
= {definicion de duplica y *}
2 : 4 : 6 : 2 * 4 : duplica [5]
= {definicion de duplica y *}
2 : 4 : 6 : 8 : 2 * 5 : duplica []
= {definicion de duplica}
2 : 4 : 6 : 8 : 10 : []
= {definicion :}
[2, 4, 6, 8, 10]

-- EXPLICACION DE COMO TRABAJA HASKELL CON LA FUNCION "expandir":
expandir :: String -> String
expandir [x] = [x]
expandir (x:xs) = x : ' ' : separa xs

separa "hola"
= {definicion de separa}
'h' : ' ' : separa "ola"
= {definicion de separa}
'h' : ' ' : ('o' : ' ' : separa "la")
= {definicion de separa}
'h' : ' ' : ('o' : ' ' : ('l' : ' ' : separa "a"))
= {definicion de separa}
'h' : ' ' : ('o' : ' ' : ('l' : ' ' : "a"))
= {deficinion de :}
"h o l a"
