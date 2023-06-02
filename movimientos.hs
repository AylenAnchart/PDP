{-Movimientos
[ Tuplas ]
Teniendo tuplas de la forma (x,y) que representan puntos en el plano:

a) Definir las funciones que indican desplazamiento en la dirección de los puntos cardinales, en una unidad, devolviendo
la nueva ubicación del punto .
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
