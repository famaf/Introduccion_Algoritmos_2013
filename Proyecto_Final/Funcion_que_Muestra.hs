-- Cambie la construccion del tipo
-- (elimine "Show" abajo para definir yo la funcion que muestra)
data Form = P Int
          | Neg Form
          | And Form Form
          deriving (Eq, Ord, Read)

mostrar :: Form -> String
mostrar (P n) = "p" ++ show n  -- show me lo pasa a String
mostrar (Neg f) = "-" ++ mostrar f
mostrar (And f g) = "(" ++ mostrar f ++ " ^ " ++ mostrar g ++ ")"

-- Aca digo que quiero mostrar las formulas usando la funcion "mostrar"
instance Show Form where
    show = mostrar


{-
Ejemplo:

*Main> let  p = And (P 1) (P 13)
*Main> let  q = Neg (P 2)
*Main> let  r = And p q
*Main> r
"((p1 ^ p13) ^ -p2)"
-}
