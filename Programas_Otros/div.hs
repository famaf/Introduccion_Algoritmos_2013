import Data.List

divG :: Int -> Int -> [Int]
divG n 0 = []
divG n k | mod n k == 0 = k : divG n (k-1)
         | otherwise = divG n (k-1)


divisores :: Int -> [Int]
divisores n = divG n n


divide x 1 = 1
divide x n | mod x (n-1) == 0 = (n-1)
           | otherwise = divide x (n-1)


lista :: Int -> Int -> [Int]
lista x 1 = []
lista x y = divide x y : lista x (y-1)


sacarrepetidos :: [Int] -> [Int]
sacarreperidos [] = []
sacarrepetidos [x] = [x]
sacarrepetidos [x, y] = [x, y]
sacarrepetidos (x:y:xs) | x == y = x : sacarrepetidos xs
                        | otherwise = x : sacarrepetidos (y:xs)


opt :: Int -> [Int]
opt x = (lista x x)
