--Devolver el valor absoluto de un número.

valorAbsoluto :: Float -> Float
valorAbsoluto = abs

--Recibir una cadena con la característica del estudiante y devolver la probabilidad de aprobar,
-- según la siguiente tabla:
-- vago 10%, inteligente 60%, estudioso 100%, cualquier otro 50%

data Estudiante = Estudiante {
    característica :: String,
    probabilidad :: Int
} deriving (Show, Eq);

vago = Estudiante "seDuermeEnLasClases" 10
inteligente = Estudiante "entiendeRapido" 60
estudioso = Estudiante "estudiaTodoElDia" 100
restante = Estudiante "personasNormales" 50

probabilidadDeAprobar :: Estudiante -> Int
probabilidadDeAprobar = probabilidad

--Dados dos elementos, devolver el menor de ambos

minimo :: (Int, Int) -> Int
minimo (unNro, otroNro)= min unNro otroNro --llamar a la función como: minimo (1,2)

--Averiguar la cantidad de días de un año, contemplando si es un año bisiesto. Un año es bisiesto si es divisible  
--entre 4, a menos que sea divisible entre 100. Sin embargo, si un año es divisible entre 400 (por más que sea 
--divisible por 100), también resulta bisiesto.

esBisiesto :: Int -> Bool
esBisiesto anio = (anio `mod` 4 == 0 && anio `mod` 100 /= 0) || (anio `mod` 400 == 0)

cantDias :: Int -> Int
cantDias anio | esBisiesto anio = 366
              | otherwise = 365


{-
Repostería funcional
[ Tuplas, listas, sinónimos de tipo ]
Una tradicional y reconocida repostería fabrica exquisitas tortas que se consumen tanto en el mercado interno como a nivel internacional. 
Las tortas son de un sabor determinado (Ej: chocolate) y de cada una se conoce la cantidad de dicho ingrediente. 
Además se sabe la cantidad de harina, la cantidad de huevos y el total de calorías.-}
{-
data Torta = Torta {
    sabor :: Sabor,
    harina :: Int,
    huevos :: Int,
    calorias :: Int
} deriving (Show, Eq)

data Sabor = Sabor {
    ingrediente :: String,
    cantidad :: Int
} deriving (Show, Eq)
-}

{-}
1) Saber si una torta es tradicional. Para esto se tiene que cumplir lo siguiente:
Si es una torta de chocolate la cantidad de este ingrediente tiene que superar el doble de la cantidad de harina.
Si es de frutilla la cantidad de huevos debe ser exactamente 3.
Si es de mascarpone la cantidad de dicho ingrediente debe estar entre 300 y 500 gramos.
> esTradicional (("mascarpone", 250), 40, 1, 100)
False-}

esTradicional :: Torta -> Bool
esTradicional torta = esDeChocolateTradicional torta || esDeFrutillaTradicional torta || esDeMascarponeTradicional torta

esDeChocolateTradicional :: Torta -> Bool
esDeChocolateTradicional torta = ingrediente (sabor torta) == "Chocolate" && cantidad (sabor torta) >= harina torta * 2

esDeFrutillaTradicional :: Torta -> Bool
esDeFrutillaTradicional torta = ingrediente (sabor torta) == "Frutilla" && huevos torta == 3

esDeMascarponeTradicional :: Torta -> Bool
esDeMascarponeTradicional torta = ingrediente (sabor torta) == "Mascarpone" && cantidad (sabor torta) > 300 && cantidad (sabor torta) < 500

{-
ghci> esDeChocolateTradicional (Torta (Sabor "Chocolate" 100) 50 1 3)
True
ghci> esDeFrutillaTradicional (Torta (Sabor "Frutilla" 100) 3 4 5)
False
ghci> esDeFrutillaTradicional (Torta (Sabor "Frutilla" 100) 4 3 5)
True
ghci> esDeMascarponeTradicional (Torta (Sabor "Mascarpone" 400) 4 3 5)
True
ghci> esDeMascarponeTradicional (Torta (Sabor "Mascarpone" 200) 4 3 5)
False
(Torta (Sabor "Vainilla" 200) 4 3 5)

(Torta (Sabor "Xd" 1) 1 1 1)


ghci> esExotica (Torta (Sabor "Mascarpone" 200) 4 3 5)
True
ghci> esExotica (Torta (Sabor "Xd" 1) 1 1 1)          
False
-}

{- (Torta (Sabor "manzana" 200) 80 2 20) (Torta (Sabor "mandarina" 250) 100 3 20) -}

{-

2) Saber si una torta es de sabor exótico. Esto sucede si no es tradicional, el sabor supera las 3 letras y 
comienza con una vocal.
> esExotica (("anana", 30), 100, 3, 50)
True-}

esExotica :: Torta -> Bool
esExotica torta = not (esTradicional torta) && (longitudDelSabor torta) > 3

longitudDelSabor :: Torta -> Int
longitudDelSabor torta = length (ingrediente (sabor torta))

{-}
3) Saber si una torta es más sana que otra. Una torta es más sana que otra si la cantidad de calorías es menor a la otra, y 
en caso de que tengan la misma cantidad de calorías, es más sana la que tiene la menor cantidad de harina.
> esMasSana (("manzana", 200), 80, 2, 20) (("mandarina", 250), 100, 3, 20)
True-}

tortaMasSana :: Torta -> Torta -> Bool
tortaMasSana torta1 torta2 = calorias torta1 < calorias torta2 || ((calorias torta1 == calorias torta2) && (harina torta1 < harina torta2))
--entiendase por torta1 la que deberia ser más sana

{-}
4) Dada una torta y una decoración, decorar la torta con dicha decoración.
type Decoración = (Ingrediente, Cantidad, Calorias)

> agregarSabor (Torta (Sabor "crema" 100)  300 3 40) (Decoracion "frutilla" 50 5)
(("crema-frutilla", 150), 300, 3, 45)
-}
data Torta = Torta {
    sabor :: Sabor,
    harina :: Int,
    huevos :: Int,
    calorias :: Int
} deriving (Show, Eq)

data Sabor = Sabor {
    ingrediente :: String,
    cantidad :: Int
} deriving (Show, Eq)

data Decoracion = Decoracion {
    nuevoSabor :: Sabor,
    caloriasDecoracion :: Int
}


crearDecoracion :: Torta -> Decoracion -> Torta
crearDecoracion torta decoracion = torta {
    sabor = agregarSabor (sabor torta) decoracion, 
    calorias = calorias torta + caloriasDecoracion decoracion
}

agregarSabor :: Sabor -> Decoracion -> Sabor
agregarSabor sabor decoracion = sabor { 
    ingrediente = ingrediente sabor ++ " y " ++ ingrediente (nuevoSabor decoracion), 
    cantidad = cantidad sabor + cantidad (nuevoSabor decoracion)
}

{-}
Si resolvemos este mismo problema en el paradigma imperativo, ¿qué diferencias conceptuales existen entre una 
y otra solución?



5) Definir el tipo de cada una de las funciones principales, aprovechando el uso de sinónimos de tipo.
-}


