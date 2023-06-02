{-Empleados
[ Tuplas, listas ]
Teniendo tuplas que representan a un empleado, con su nombre, edad, sexo y una lista con las sucursales por las que pasó,
ordenada cronológicamente.-}

data Empleado = Empleado {
    nombreEmpleado :: String,
    edad :: Int,
    sexo :: Char,
    sucursales :: [String]
}

pepe :: Empleado
pepe = Empleado "Pepe" 34 'M' ["Campana","Zarate"]

virginia :: Empleado
virginia = Empleado "Virginia" 60 'F' ["Casa Matriz"]

juan :: Empleado
juan = Empleado "Juan" 60 'M' ["Campana"]

maria :: Empleado
maria = Empleado "Maria" 57 'F' ["Campana"]

{-}

a) Devolver la primera sucursal en la que trabajó el empleado
> nombre ("Pepe", 34, ‘M’, ["Campana", "Zarate"])
"Campana"
-}
primeraSucursal :: Empleado -> String
primeraSucursal = head . sucursales --devuelve el primer elemento de una lista

{-}
b) Averiguar si el empleado trabaja actualmente en la casa matriz de la empresa
> casaMatriz ("Lucho", 60, 'M', ["Campana", "Casa Matriz"])
True
-}

trabajaEnCasaMatriz :: Empleado -> Bool
trabajaEnCasaMatriz = (elem "Casa Matriz").sucursales

{-}
c) Indica la cantidad de años que le falta a un empleado para jubilarse (las mujeres se jubilan a los 60 años y los 
varones a los 65).
> paraJubilarse ("Lola", 45, 'F', ["Buenos Aires", "Campana"])
15
-}

paraJubilarse :: Empleado -> Int
paraJubilarse empleado
    | sexo empleado == 'F' = 60 - edad empleado
    | sexo empleado == 'M' = 65 - edad empleado
    | otherwise = error "Género no válido"

{-}
d) Saber si dos empleados pasaron por la misma sucursal.
> mismaSucursal ("Pepe", 34, ‘M’, ["Campana", "Zarate"]) ("Lola", 45, 'F', ["Buenos Aires", "Campana"])
True-}


mismasSucursales :: Empleado -> Empleado -> Bool
mismasSucursales empleado1 empleado2 = any (`elem` sucursales empleado2) (sucursales empleado1) -- La función any es
--una función de orden superior en Haskell que toma un predicado y una lista, y devuelve True si al menos un elemento 
--de la lista cumple con el predicado, y False en caso contrario.

{-}

e) Saber si dos empleados pueden salir juntos, para lo cual deben ser de diferente sexo y le tienen que faltar a ambos 
más de 10 años para jubilarse. Como requisito adicional, deben haber trabajado en la misma sucursal  o que alguno de 
ellos estar actualmente en la casa matriz. 
-}

puedenSalir :: Empleado -> Empleado -> Bool
puedenSalir empleado1 empleado2 = diferenteSexo empleado1 empleado2 && (paraJubilarse empleado1 < 10) && (paraJubilarse empleado2 < 10) && (mismasSucursales empleado1 empleado2 || ((trabajaEnCasaMatriz empleado1 || trabajaEnCasaMatriz empleado2)))

diferenteSexo :: Empleado -> Empleado -> Bool
diferenteSexo empleado1 empleado2 = sexo empleado1 /= sexo empleado2
