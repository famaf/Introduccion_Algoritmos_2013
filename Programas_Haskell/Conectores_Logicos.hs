-- CONECTIVOS LOGICOS

-- Implicación ( => )

(-->) :: Bool -> Bool -> Bool
(-->) p q | p == True && q == True = True
        | p == True && q == False = False
        | p == False && q == True = True
        | p == False && q == False = True


-- Consecuente ( <= )

(<--) :: Bool -> Bool -> Bool
(<--) p q | p == True && q == True = True
        | p == True && q == False = True
        | p == False && q == True = False
        | p == False && q == False = True
