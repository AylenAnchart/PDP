{-A lo largo de los años, los streamings multimedia se han hecho más y más famosos, llegando incluso a superar la 
misma televisión. Netflis se ha consagrado como la empresa más importante de este rubro gracias a su gran catálogo y 
económico precio. En esta ocasión nos han pedido que modelemos las series que ofrecerán en su catálogo, para así darle 
una buena experiencia al usuario. Sabemos que una serie se compone de un nombre, un género, una duración total en horas, 
una cantidad de temporadas, una calificación (en una escala de 5 estrellas) y si es original de Netflis.-}

data Serie = UnaSerie {
    nombre :: String,
    genero :: String,
    duracion :: Int,
    cantTemporadas :: Int,
    calificaciones :: [Int],
    esOriginalDeNetflis :: Bool
} deriving (Eq, Show)

tioGolpetazo = UnaSerie {
    nombre = "One punch man",
    genero = "Monito chino",
    duracion = 24,
    cantTemporadas = 1,
    calificaciones = [5],
    esOriginalDeNetflis = False
}
 
cosasExtranias = UnaSerie {
    nombre = "Stranger things",
    genero = "Misterio",
    duracion = 50,
    cantTemporadas = 2,
    calificaciones = [3,3],
    esOriginalDeNetflis = True
}

dbs = UnaSerie {
    nombre = "Dragon ball supah",
    genero = "Monito chino",
    duracion = 150,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}
espejoNegro = UnaSerie {
    nombre = "Black mirror",
    genero = "Suspenso",
    duracion = 123,
    cantTemporadas = 4,
    calificaciones = [2],
    esOriginalDeNetflis = True
}

rompiendoMalo = UnaSerie {
    nombre = "Breaking Bad",
    genero = "Drama",
    duracion = 200,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}

treceRazonesPorque = UnaSerie {
    nombre = "13 reasons why",
    genero = "Drama",
    duracion = 50,
    cantTemporadas = 1,
    calificaciones = [3,3,3],
    esOriginalDeNetflis = True
}

{-Parte 1: Listas básicas -}

-- 1) Crear una maratón con los ejemplos dados. Una maratón es una colección de series.

type Maraton = [Serie]

maraton :: Maraton
maraton = [treceRazonesPorque, rompiendoMalo, cosasExtranias]

-- 2) Saber la cantidad de series del maratón

cantSeriesMaraton :: Maraton -> Int
cantSeriesMaraton = length 
-- 3) Saber si una serie es popular: una serie se considera popular si recibió 3 o más calificaciones.

esPopular :: Serie -> Bool
esPopular serie = length (calificaciones serie) >= 3

-- 4) Averiguar si una serie vale la pena, es decir, si tiene más de una temporada y tiene 3 o más calificaciones.

valeLaPena :: Serie -> Bool
valeLaPena serie = (cantTemporadas serie > 1) && esPopular serie

{-} 5) Saber si una maratón vale la pena: una maratón vale la pena si la primera y la última serie de la maratón valen 
la pena, o bien si está el drama "Breaking Bad", con 5 temporadas y 200 minutos, sin calificar, que no es original de 
netflis.-}

maratonValeLaPena :: Maraton -> Bool
maratonValeLaPena maraton = (valeLaPena (head maraton) && valeLaPena (last maraton)) || elem (UnaSerie "Breaking Bad" "" 200 5 [] False) maraton

-- Calcular la calificación de una serie. Es el promedio de las calificaciones recibidas, (redondeado hacia abajo) 

totalCalificaciones :: Serie -> Int
totalCalificaciones serie = length (calificaciones serie)

sumaCalificaciones :: Serie -> Int 
sumaCalificaciones serie = foldr (+) 0 (calificaciones serie)

promedioCalificaciones :: Serie -> Int
promedioCalificaciones serie = div (sumaCalificaciones serie) (totalCalificaciones serie)

{-} 8) Obtener la dispersión de las calificaciones de la serie, que es la diferencia entre la mejor y peor calificación. 
(Si todas las calificaciones son coincidentes, se deduce que la dispersión es 0), -}

dispersionCalificacion :: Serie -> Int
dispersionCalificacion serie = maximum (calificaciones serie) - minimum (calificaciones serie)

-- 9) Calificar una serie, que significa agregar una nueva calificación al final de las anteriores.

calificarUnaSerie :: Int -> Serie -> Serie
calificarUnaSerie n serie = serie {calificaciones = calificaciones serie ++ [n]}

{-Hypear una serie: cuando se hypea una serie, se alteran la primer y última calificación recibida, aumentándola en 2 
estrellas (recordá que la escala de calificación es de 5 estrellas máximo). Si la serie recibió alguna calificación de 
1 estrella, no se puede hypear.-}

hypearUnaSerie :: Serie -> Serie
hypearUnaSerie serie = serie { calificaciones = map ajustarCalificacion (calificaciones serie) }
  where
    ajustarCalificacion calificacion
      | esPrimerElemento calificacion || esUltimoElemento calificacion = min (calificacion + 2) 5
      | otherwise = calificacion

    esPrimerElemento calificacion = calificacion == head (calificaciones serie)
    esUltimoElemento calificacion = calificacion == last (calificaciones serie)







