------------------------------------------------------------------------------
---- PROGRAMA QUE RESUELVE ECUACIONES LINEALES CON DOS INCOGNITAS "X e Y" ----
------------------------------------------------------------------------------

-- PD: SE UTILIZA EL SISTEMA CON DETERMINANTES:



-- DADOS LOS VALORES DE EL SISTEMA DE ECUACIONES,
-- CALCULA EL VALOR DE LA INCOGNITA "X":
resultadox :: Fractional a => (a,a,a) -> (a,a,a) -> a
resultadox (a,b,c) (x,y,z) = ((c*y)-(z*b))/((a*y)-(x*b))


-- DADOS LOS VALORES DE EL SISTEMA DE ECUACIONES,
-- CALCULA EL VALOR DE LA INCOGNITA "Y":
resultadoy :: Fractional a => (a,a,a) -> (a,a,a) -> a
resultadoy (a,b,c) (x,y,z) = ((a*z)-(x*c))/((a*y)-(x*b))


-- DADOS LOS VALORES DE EL SISTEMA DE ECUACIONES,
-- CALCULA EL VALOR DE LA INCOGNITA "X e Y":
resultadosxy (a,b,c) (x,y,z) = "Resultados del sistema de ecuaciones:" ++ " " ++ "x = " ++ show(((c*y)-(z*b))/((a*y)-(x*b))) ++ "  ,  " ++ "y = " ++ show(((a*z)-(x*c))/((a*y)-(x*b)))
