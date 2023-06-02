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

{- En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. El peso de un pino se 
puede calcular a partir de la altura así: 3 kg x cm hasta 3 metros, 2 kg x cm arriba de los 3 metros. 
P.ej. 2 metros -> 600 kg, 5 metros -> 1000 kg.
Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, 
un pino fuera de este rango no le sirve a la fábrica.
Para esta situación:
Definir la función pesoPino, recibe la altura de un pino y devuelve su peso.
Definir la función esPesoUtil, recibe un peso en kg y devuelve True si un pino de ese peso le sirve a la fábrica, 
y False en caso contrario.
Definir la función sirvePino, recibe la altura de un pino y devuelve True si un pino de ese peso le sirve a la fábrica,
 y False en caso contrario.
Definir la función prioridadPino, que recibe la altura de un pino y en base a ella clasifica al pino:
Si la altura es exactamente igual a 2 metros, entonces el pino tiene prioridad alta.
Sino si la altura no supera los 3 metros y su peso útil es mayor a 800, tiene prioridad media.
Si el pino sirve a la empresa, entonces tiene prioridad baja.
En cualquier otro caso, se considera prioridad obsoleto y no se hará nada con él.
-}

data Pino = Pino {
    peso :: Int,
    altura :: Int
}

pinoBlanco = Pino 100 100 
pinoRojo = Pino 200 4
pinoAzul = Pino 1000 30
pinoAmarillo = Pino 1000 2
pinoRosa = Pino 200 1

pesoPino :: Int -> Int
pesoPino num | num < 3 = num * 3
             | otherwise = num * 2

esPesoUtil :: Int -> Bool
esPesoUtil num = pesoPino num > 400 && pesoPino num < 1000

sirvePino :: Int -> Bool
sirvePino = esPesoUtil.pesoPino

prioridadDelPino :: Pino -> String
prioridadDelPino pino
  | altura pino == 2 = "Alta" --pinoAmarillo
  | altura pino < 3 && esPesoUtil (pesoPino (peso pino)) = "Media" --pinoRosa
  | otherwise = "Obsoleto"  --el resto de los pinos

{-Foringa
[ Tipos de datos básicos, guardas ]

En un famoso foro de intercambio legal de información legítima y software aprobado
por la ley, se quiere hacer un agregado al sistema de puntuación. Hay tres niveles de
usuarios (newbie, intermedio y avanzado) y todos pueden puntuar. Los usuarios newbies
otorgan como máximo 1 punto, los intermedios como máximo 5 puntos, y los avanzados
como máximo 10 puntos.

Se tiene la siguiente información:

anioIngreso "george" = 2013
anioIngreso "sexy99" = 2012
anioIngreso "boca_cabj" = 2011
anioIngreso "jroman" = 2010

Se pide:
1 La antigüedad de un usuario
> antiguedad "sexy99"
1
-}

data Niveles = Niveles {
    nivel :: String,
    puntos :: Int
}

newbie = Niveles "newbie" 1
intermedios = Niveles "intermedios" 5
avanzados = Niveles "avanzados" 10

--Antiguedad de un usuario

data Usuario = Usuario {
    nombre :: String,
    anioDeIngreso :: Int
}

george = Usuario "George" 2013
sexy99 = Usuario "Sexy99" 2012
boca_cabj = Usuario "boca_cabj" 2011
jroman = Usuario "jroman" 2010
pepito = Usuario "pepito" 2023
alberto = Usuario "alberto" 2020


antiguedad :: Int -> Usuario -> Int
antiguedad anioActual usuario = anioActual - anioDeIngreso usuario 

{-}
2 Los puntos base de un usuario, sabiendo que se calculan como la antigüedad por la
longitud del nombre.
> puntosBase "sexy99"
6
-}

puntosBase :: Int -> Usuario -> Int
puntosBase anioActual usuario = antiguedad anioActual usuario * length (nombre usuario)

{-}
3 El nivel de un usuario, sabiendo que si tiene menos de un año de antigüedad es
“newbie”, si tiene menos de 50 puntos base es “intermedio”, y sino es “avanzado”.
> nivel "sexy99"
"intermedio"
-}

nivelDeUnUsuario :: Int -> Usuario -> String
nivelDeUnUsuario anioActual usuario | antiguedad anioActual usuario < 1 = "Newbie" --pepito
                                    | puntosBase anioActual usuario < 50 = "Intermedio" --alberto
                                    | otherwise = "Avanzado" -- el resto de usuarios



{-
Palabras
[ Listas, Listas por comprensión/orden superior/recursividad ]
a) Obtener la cantidad de vocales que tiene una palabra

b) Devolver si es cierto que una frase tiene más consonantes que vocales

c) Armar la sigla de una frase compuesta por varias palabras
-}

--Cantidad de vocales de una palabra

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiou"

obtenerVocales :: [Char] -> [Char]
obtenerVocales = filter esVocal

cantVocales :: [Char] -> Int
cantVocales = length.obtenerVocales

obtenerConsonantes :: [Char] -> [Char]
obtenerConsonantes = filter (not.esVocal)

cantConsonante :: [Char] -> Int
cantConsonante = length.obtenerConsonantes

{-Movimientos
[ Tuplas ]
Teniendo tuplas de la forma (x,y) que representan puntos en el plano:

a) Definir las funciones que indican desplazamiento en la dirección de los puntos cardinales, en una unidad, devolviendo la nueva ubicación del punto .
> norte (3,9)
(4,9)
> este (3,9)
(3,10)

b) Utilizando las anteriores, definir las funciones correspondientes a las direcciones intermedias (noreste,noroeste, sudeste, sudoeste)
> noreste (4,9)
(5,10)

-}

norte :: (Int,Int) -> (Int, Int)
norte (x,y) = (x + 1, y)

este :: (Int, Int) -> (Int, Int)
este (x,y) = (x, y + 1)

noreste :: (Int, Int) -> (Int, Int)
noreste = este . norte

{-Notas
[ Tuplas ]

Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1, nota2), p.ej. un patito en 
el 1ro y un 7 en el 2do se representan mediante el par (2,7).

A partir de esto:
1. Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 4, False en caso contrario. 
No vale usar guardas.
2. Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo.
3. Definir la función promociono, que indica si promocionó, para eso las dos notas tienen que sumar al menos 14 y además 
debe haberse sacado al menos 6 en los dos parciales.
-}
type Notas = (Float, Float)

promedioNotas :: Notas -> Float
promedioNotas (nota1, nota2) = (nota1 + nota2) / 2 -- NO OLVIDAR QUE EN TUPLAS LA VARIABLE SE ESCRIBE TAMBIÉN COMO TUPLA

esNotaBochazo :: Notas -> Bool
esNotaBochazo notas = promedioNotas notas < 4

aprobo :: Notas -> Bool
aprobo = not.esNotaBochazo

promociono :: Notas -> Bool
promociono (nota1, nota2) = nota1 >= 6 && nota2 >= 6 && nota1 + nota2 >= 14

{-Siguiendo con el dominio del ejercicio anterior, tenemos ahora dos parciales con dos recuperatorios, lo representamos 
mediante un par de pares:

((parc1, parc2), (recup1, recup2))

Si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente. Observamos que con 
la codificación elegida, siempre la mejor nota es el máximo entre nota del parcial y nota del recuperatorio. Considerar 
que vale recuperar para promocionar. En este ejercicio vale usar las funciones que se definieron para el anterior.-}
{-}
type Parciales = ((Int, Int), (Int,Int))


rindioRecuperatorioTotal :: ((Int, Int), (Int, Int)) -> Bool
rindioRecuperatorioTotal ((parc1, parc2),(recup1, recup2)) = recup1 /= -1 || recup2 /= -1

rindioRecuperatorio :: ((Int, Int), (Int, Int)) -> (Bool, Bool)
rindioRecuperatorio ((parc1, parc2), (recup1, recup2)) = (recup1 /= -1 && recup2 /= -1) = (True, True) || (recup1 = -1 && recup2 /= -1) = (False, True) || (recup1 /= -1 && recup2 = -1) = (True, False) || (recup1 = -1 && recup2 = -1) = (False, False)


separarParciales :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
separarParciales ((parc1, parc2),(recup1, recup2)) = ((parc1, recup1), (parc2, recup2))

-}
{-4. Definir la función notasFinales que recibe un par de pares y devuelve el par que corresponde a las notas finales 
del alumno para el 1er y el 2do parcial. P.ej.

Main> notasFinales ((2,7),(6,-1))
(6,7)
Main> notasFinales ((2,2),(6,2))
(6,2)
Main> notasFinales ((8,7),(-1,-1))
(8,7)

5. Definir la función recuperoDeGusto que dado el par de pares que representa a un alumno, devuelve True si el alumno,
 pudiendo promocionar con los parciales (o sea sin recup.), igual rindió al menos un recup. Vale definir funciones 
 auxiliares.
-}

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

pepe = Empleado "Pepe" 34 'M' ["Campana","Zarate"]

virginia = Empleado "Virginia" 60 'F' ["Casa Matriz"]

juan = Empleado "Juan" 60 'M' ["Campana"]

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

{-Aguante arbolito
[Tuplas, Listas, Orden Superior]

En una plantación de árboles de una reserva natural nos contrataron para realizar un sistema que permita mejorar 
el seguimiento y control de todos los árboles.
Un árbol está representado por una tupla de 4 elementos (especie, metros de altura, metros de ancho, vitalidad). En 
la reserva encontramos, por ejemplo, los siguientes árboles:
 
arbolesEn "la reserva" = [("jacaranda", 6, 1, 1.4), ("pino", 5, 3, 1.9), ("eucalipto", 5, 4, 0.7), 
("jacaranda", 10, 2, 1.0), ("cerezo", 7, 11, 0.9), ("ombu", 8, 10, 2.1)]
 
a)    Obtener los nombres de las especies de los árboles frondosos, que son los que su altura está entre 6 y 15 metros 
y la anchura supera la altura. 
nombresFrondosos (arbolesEn "la reserva")
["cerezo", "ombu"]-}

data Arbol = Arbol {
    especie :: String,
    alturaMetro :: Int,
    ancho :: Int,
    vitalidad :: Float
} deriving (Show, Eq)

jacaranda = Arbol "Jacaranda" 6 1 1.4
pino = Arbol "Pino" 5 3 1.9
eucalipto = Arbol "Eucalipto" 5 4 0.7
cerezo = Arbol "cerezo" 7 11 0.9
ombu = Arbol "ombu" 8 10 2.1

arbolesFrondosos :: [Arbol] -> [String]
arbolesFrondosos arboles = [especie arbol | arbol <- arboles, alturaMetro arbol >= 6 && alturaMetro arbol <= 14]

{-}

b)    Saber si todos los árboles frondosos tienen buena vitalidad, es decir, mayor a 1.
todosLosFrondososSonVitales (arbolesEn "la reserva")
False	
(el cerezo es frondoso pero tiene vitalidad 0.9) -}

todosLosFrondososSonVitales :: [Arbol] -> Bool
todosLosFrondososSonVitales = all (\arbol -> vitalidad arbol > 1) 

{-}
 
c) Implementar los siguientes factores climáticos que pueden modificar los árboles.
lluvia milimetros unArbol, al llover aumenta en un 1 metro la altura del árbol y además aumenta la vitalidad del árbol en un porcentaje igual a los milímetros recibidos.
temperatura grados unArbol, si es una temperatura bajo cero disminuye la vitalidad a la mitad, si es de más de 40 también disminuye vitalidad pero en un 40% y entre medio, no lo afecta.
granizo unArbol, disminuye a la mitad el ancho y alto del árbol.-}

lluvia :: Float -> Arbol -> Arbol
lluvia milimetros arbol = arbol { alturaMetro = alturaMetro arbol + 1, vitalidad = vitalidad arbol * (1 + milimetros / 100) }

temperaturaGrados :: Float -> Arbol -> Arbol
temperaturaGrados temperatura arbol
  | temperatura < 0 = arbol { vitalidad = vitalidad arbol / 2 }
  | temperatura > 40 = arbol {vitalidad = vitalidad arbol * 0.4}
  | otherwise = arbol

granizo :: Arbol -> Arbol
granizo arbol = arbol { ancho = div (ancho arbol) 2, alturaMetro = div (alturaMetro arbol) 2 }

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


