data Form = P Int
          | Neg Form
          | And Form Form
          | Or Form Form
          | Impl Form Form
          | Equiv Form Form

-- DICE CUANTAS SON LA CANTIDAD DE VARIABLES DE UNA FORMULA:
contvar :: Form -> Int
contvar (P n) = 1
contvar (Neg f) = contvar f
contvar (And f g) = contvar f + contvar g
contvar (Or f g) = contvar f + contvar g
contvar (Impl f g) = contvar f + contvar g
contvar (Equiv f g) = contvar f + contvar g


-- DICE CUALES ES LA VARIABLE MAS GRANDE DE UNA FORMULA:
mayorvar :: Form -> Int
mayorvar (P n) = n
mayorvar (Neg f) = mayorvar f
mayorvar (And f g) = max (mayorvar f) (mayorvar g)
mayorvar (Or f g) = max (mayorvar f) (mayorvar g)
mayorvar (Impl f g) = max (mayorvar f) (mayorvar g)
mayorvar (Equiv f g) = max (mayorvar f) (mayorvar g)
