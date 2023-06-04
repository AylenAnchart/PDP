{- Una organización sin fines de lucro nos pide un sistema que les permita conocer los efectos que producen 
los parques de atracciones en la sociedad. 
Se ha decidido implementar dicho sistema utilizando nuestros conocimientos en Haskell, utilizando el paradigma funcional.
-}

{- Modelado del Sistema

De las personas se conoce su nombre, nivel de orientación, nivel de emoción y el nivel de cultura. Modele dicha 
información y cree a las siguientes personas en el sistema:

Isabel tiene en sus tres niveles 100.
Los niveles de Mario son, nivel de orientación es 50, emoción 120 y cultura 30.
Teresa tiene todos sus niveles en 0 previo a ingresar a los parques.

personas = [isabel, mario, teresa]

De los parques se conoce, su temática y las atracciones que posee según dicho tema.
Modele el parque y cree los siguientes parques en el sistema:

El parque de Diversiones tiene una Montaña Rusa de 100, Caida Libre de 60.
El parque Temático tiene un Mundo de Maya y un Cine 4D.

Nota: Más adelante en el enunciado entraremos en detalle sobre cada una de estas atracciones

parques = [ diversiones, tematico]

Además los visitantes a los parques tendrán la posibilidad de tomar algún refresco, lo cual 
duplica su nivel de orientación.
-}

data Persona = Persona {
    nombre :: String,
    orientacion :: Float,
    emocion :: Float,
    cultura :: Float
} deriving (Show, Eq)

isabel = Persona "Isabel" 100 100 100
mario = Persona "Mario" 50 120 30
teresa = Persona "Teresa" 0 0 0

data Parques = Diversiones Tematica Atracciones | Tematico Tematica Atracciones deriving (Show, Eq)

type Tematica = String
type Atracciones = [(String, Metros)]
type Metros = Float



diversiones = Diversiones "Diversiones" [("Montania Rusa", 100), ("Caida Libre", 60)]
tematico = Tematico "Tematico" [("Mundo Maya", 0), ("Cine 4D", 0)]

tomarRefresco :: Persona -> Persona
tomarRefresco persona = persona {orientacion = orientacion persona * 2}

{-Funcionalidades solicitadas por la organización

Nota: Escribir para todas las funciones sus tipos.

1) Desarrollar la función atracciones que, dada una lista de parques permita conocer el conjunto de todas las atracciones.

>atracciones parques
[montañaRusa 100, caidaLibre 60 , mundoMaya, cine4D]
-}

atracciones :: [Parques] -> [String]
atracciones parques = concatMap obtenerAtracciones parques

obtenerAtracciones :: Parques -> [String]
obtenerAtracciones (Diversiones _ atracciones) = map fst atracciones
obtenerAtracciones (Tematico _ atracciones) = map fst atracciones


{-2) Dadas las siguientes atracciones, conocer el estado de la persona luego de visitar cada atracción:

montañaRusa: dada una velocidad, produce una disminución de la orientación en un 15% de la velocidad
caidaLibre: dado unos metros de caída, produce un aumento en la emoción de la persona en un 20% de los metros de caída.
mundoMaya: produce un incremento de un 10% en nivel de emoción y un incremento de un 20% en el nivel de cultura de la 
persona.
cine4D: no produce efecto sobre la persona.-}

visitoMontaniaRusa :: Persona -> Persona
visitoMontaniaRusa persona = persona {orientacion = orientacion persona - (orientacion persona * 0.15)}

visitoCaidaLibre :: Metros -> Persona -> Persona
visitoCaidaLibre metros persona = persona {emocion = emocion persona + (metros * 0.20)}

visitoMundoMaya :: Persona -> Persona
visitoMundoMaya persona = persona {emocion = emocion persona + (emocion persona * 0.10), cultura = cultura persona + (cultura persona * 0.20) }

{- 3) Desarrollar la función refrescar que dada una atracción y una persona hace que la persona asista a 
una atracción y cuando sale tome un refresco.-}

aplicarAtraccion :: Parques -> Persona -> Persona
aplicarAtraccion (Diversiones _ [("Montania Rusa", _ )]) persona = visitoMontaniaRusa persona
aplicarAtraccion (Diversiones _ [("Caida Libre", metros)]) persona = visitoCaidaLibre metros persona 
aplicarAtraccion (Tematico _ [("Mundo Maya", _ )]) persona = visitoMundoMaya persona
aplicarAtraccion (Tematico _ [("Cine 4D",_ )]) persona = persona
aplicarAtraccion _ persona = persona 

refrescar :: Parques -> Persona -> Persona
refrescar parque persona = tomarRefresco (aplicarAtraccion parque persona)

{- 4) Dado un conjunto de personas, conocer los nombres de aquellas que tienen un nivel de orientación mayor a N, 
luego de que asistieron a la montaña rusa, tomaron un refresco y asistieron a mundoMaya (usar composición).-}


personasConOrientacionMayorAN :: Float -> [Persona] -> [Persona]
personasConOrientacionMayorAN n personas = filter (\persona -> orientacion persona > n) personas

personasTransformadas :: [Persona] -> [Persona]
personasTransformadas = map  (visitoMundoMaya  . tomarRefresco . visitoMontaniaRusa)

orientacionMayor :: Float -> [Persona] -> [Persona]
orientacionMayor n = (personasConOrientacionMayorAN n).personasTransformadas

{-5) ¿Qué pasaría si invocamos la función el punto anterior con una lista de personas infinita? Explicar cúal 
sería el comportamiento del programa.-}

{- El programa se ejecutará indefinidamente sin producir un resultado -}

{-6) Dada una persona y un conjunto de atracciones, determinar cómo queda la persona luego de pasar por todas 
las atracciones. Tener en cuenta que luego de cada atracción la persona se toma un refresco (no usar recursividad).-}

aplicarTodasAtracciones :: Persona -> [Parques] -> Persona
aplicarTodasAtracciones = foldl (\ p atraccion -> refrescar atraccion p)

{-7) Repetir el comportamiento del punto 6, pero en este caso solo toman el refresco aquellos cuyo nivel de orientación 
es menor a 10 (usar recursividad).-}

aplicarTodasAtraccionesRecursivo :: Persona -> [Parques] -> Persona
aplicarTodasAtraccionesRecursivo persona [] = persona
aplicarTodasAtraccionesRecursivo persona (atraccion:atracciones)
  | orientacion persona < 10 = aplicarTodasAtraccionesRecursivo (refrescar atraccion persona) atracciones
  | otherwise = aplicarTodasAtraccionesRecursivo persona atracciones

{-8) Dada una persona y un conjunto de atracciones determinar si el nivel de dicha persona  creció luego de asistir a 
todas las atracciones, analizar según:
a) nivel de orientación
b) nivel de emoción
c) nivel de cultura

-}

aumentaronSusNiveles :: Persona -> [Parques] -> Bool
aumentaronSusNiveles persona atracciones = compararNiveles (aplicarTodasAtraccionesRecursivo persona atracciones) persona


compararNiveles :: Persona -> Persona -> Bool
compararNiveles p1 p2 = orientacion p1 > orientacion p2 && emocion p1 > emocion p2 && cultura p1 > cultura p2

