encode :: String -> String -> String
encode "" "" = "Indique Codigo y Texto"
encode "qwerty" "" = "Indique Texto"
encode "" (x:xs) = "Indique Codigo"
encode "qwerty" (x:xs) = encoded (x:xs)

decode :: String -> String -> String
decode "" "" = "Indique Codigo y Texto"
decode "qwerty" "" = "Indique Texto"
decode "" (x:xs) = "Indique Codigo"
decode "qwerty" (x:xs) = decoded (x:xs)

encoded :: String -> String
encoded "" = ""
encoded (x:xs) | x == 'a' = 'q' : encoded xs
               | x == 'b' = 'w' : encoded xs
               | x == 'c' = 'e' : encoded xs
               | x == 'd' = 'r' : encoded xs
               | x == 'e' = 't' : encoded xs
               | x == 'f' = 'y' : encoded xs
               | x == 'g' = 'u' : encoded xs
               | x == 'h' = 'i' : encoded xs
               | x == 'i' = 'o' : encoded xs
               | x == 'j' = 'p' : encoded xs
               | x == 'k' = 'a' : encoded xs
               | x == 'l' = 's' : encoded xs
               | x == 'm' = 'd' : encoded xs
               | x == 'n' = 'f' : encoded xs
               | x == 'ñ' = 'g' : encoded xs
               | x == 'o' = 'h' : encoded xs
               | x == 'p' = 'j' : encoded xs
               | x == 'q' = 'k' : encoded xs
               | x == 'r' = 'l' : encoded xs
               | x == 's' = 'N' : encoded xs
               | x == 't' = 'z' : encoded xs
               | x == 'u' = 'x' : encoded xs
               | x == 'v' = 'c' : encoded xs
               | x == 'w' = 'v' : encoded xs
               | x == 'x' = 'b' : encoded xs
               | x == 'y' = 'n' : encoded xs
               | x == 'z' = 'm' : encoded xs
               | otherwise = x : encoded xs


decoded :: String -> String
decoded "" = ""
decoded (x:xs) | x == 'q' = 'a' : decoded xs
               | x == 'w' = 'b' : decoded xs
               | x == 'e' = 'c' : decoded xs
               | x == 'r' = 'd' : decoded xs
               | x == 't' = 'e' : decoded xs
               | x == 'y' = 'f' : decoded xs
               | x == 'u' = 'g' : decoded xs
               | x == 'i' = 'h' : decoded xs
               | x == 'o' = 'i' : decoded xs
               | x == 'p' = 'j' : decoded xs
               | x == 'a' = 'k' : decoded xs
               | x == 's' = 'l' : decoded xs
               | x == 'd' = 'm' : decoded xs
               | x == 'f' = 'n' : decoded xs
               | x == 'g' = 'N' : decoded xs
               | x == 'h' = 'o' : decoded xs
               | x == 'j' = 'p' : decoded xs
               | x == 'k' = 'q' : decoded xs
               | x == 'l' = 'r' : decoded xs
               | x == 'ñ' = 's' : decoded xs
               | x == 'z' = 't' : decoded xs
               | x == 'x' = 'u' : decoded xs
               | x == 'c' = 'v' : decoded xs
               | x == 'v' = 'w' : decoded xs
               | x == 'b' = 'x' : decoded xs
               | x == 'n' = 'y' : decoded xs
               | x == 'm' = 'z' : decoded xs
               | otherwise = x : decoded xs
