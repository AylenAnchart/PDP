{-Un club nos contrató para hacer un sistema de seguimiento de sus socios que practican gimnasia artística,  de los 
cuales  se conoce sus deportistas y los ejercicios que realizan.
De cada gimnasta se sabe su nombre, su nivel de energía, el nivel de equilibrio, la flexibilidad,  el nivel de fuerza 
física y las habilidades que posee, es decir los ejercicios que sabe realizar muy bien.  De cada ejercicio se conoce los 
efectos que produce en el gimnasta.
medialuna aumenta en 5 unidades el nivel de equilibrio del gimnasta.
rolAdelante dependiendo de la velocidad con que lo realiza,  aumenta  la energía del gimnasta en la mitad de la 
velocidad.
vertical aumenta en 7 unidades la fuerza física del gimnasta.
saltoConSoga, disminuye la energía en la mitad de la cantidad de saltos  y aumenta la fuerza física tanto como la 
cantidad de saltos que realiza.
saltoMortal aumenta la fuerza física del gimnasta en tanto como la altura del mismo y la flexibilidad en la mitad del 
nivel del impulso del salto.

Aprovechando los conceptos que vimos en clase de aplicación parcial, composición de funciones, orden superior, listas 
por comprensión definir tanto las funciones principales como todas las auxiliares que permitan conocer:
-}

data Gimnasta = Gimnasta {
    nombre :: String,
    energia :: Int,
    equilibrio :: Int,
    flexibilidad :: Int,
    fuerza :: Int,
    habilidades :: [String]
} deriving (Show, Eq)

poseeHabilidadMedialuna :: Gimnasta -> Bool
poseeHabilidadMedialuna gimnasta = elem "medialuna" (habilidades gimnasta)

actualizarGimnasta :: Gimnasta -> Gimnasta
actualizarGimnasta gimnasta = gimnasta {equilibrio = equilibrio gimnasta + 5}

mediaLuna :: Gimnasta -> Gimnasta
mediaLuna gimnasta
  | poseeHabilidadMedialuna gimnasta = actualizarGimnasta gimnasta
  | otherwise = gimnasta



