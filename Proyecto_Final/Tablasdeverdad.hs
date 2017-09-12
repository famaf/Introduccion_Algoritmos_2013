-- DA LA TABLA DE VERDAD DE "n" CANTIDAD DE VARIABLES:
tablaverdad :: Int -> [[Bool]]
tablaverdad 1 = [[True], [False]]
tablaverdad n = auxiliar (tablaverdad (n-1))

auxiliar :: [[Bool]] -> [[Bool]]
auxiliar [] = []
auxiliar (x:xs) = (x ++ [True]) : (x ++ [False]) : (auxiliar xs)

{-
Ejemplo de como Trabaja la Funcion:

tablaverdad 3
= {definicion de tablaverdad}
auxiliar (tablaverdad 2)
= {definicion de tablaverdad}
auxiliar (auxiliar (tablaverdad 1))
= {definicion de tablaverdad}
auxiliar (auxiliar [[True], [False]])
= {definicion de auxiliar}
auxiliar ([True, True], [True, False] : (auxiliar [[False]]))
= {definicion de auxiliar}
auxiliar ([True, True] : [True, False] : ([False, True] : [False, False] : auxiliar []))
= {definicion de auxiliar}
auxiliar ([True, True] : [True, False] : [False, True] : [False, False] : [])
= {definicion de auxiliar}
auxiliar ([[True, True], [True, False], [False, True], [False, False]])
= {definicion de auxiliar}
[True, True, True] : [True, True, False] : (auxiliar [[True, False], [False, True], [False, False]])
= {definicion de auxiliar}
[True, True, True] : [True, True, False] : [True, False, True] : [True, False, False] : (auxiliar [[False, True], [False, False]])
= {definicion de auxiliar}
[True, True, True] : [True, True, False] : [True, False, True] : [True, False, False] : [False, True, True] : [False, True, False] : (auxiliar [[False, False]]) 
= {definicion de auxiliar}
[True, True, True] : [True, True, False] : [True, False, True] : [True, False, False] : [False, True, True] : [False, True, False] : [False, False, True] : [False, False, False] : (auxiliar [])
= {definicion de auxiliar}
[True, True, True] : [True, True, False] : [True, False, True] : [True, False, False] : [False, True, True] : [False, True, False] : [False, False, True] : [False, False, False] : []
= {definicion de :}
[[True, True, True] : [True, True, False] : [True, False, True] : [True, False, False] : [False, True, True] : [False, True, False] : [False, False, True] : [False, False, False]]
-}
