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

jacaranda :: Arbol
jacaranda = Arbol "Jacaranda" 6 1 1.4
pino :: Arbol
pino = Arbol "Pino" 5 3 1.9
eucalipto :: Arbol
eucalipto = Arbol "Eucalipto" 5 4 0.7
cerezo :: Arbol
cerezo = Arbol "cerezo" 7 11 0.9
ombu :: Arbol
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