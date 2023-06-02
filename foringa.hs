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
