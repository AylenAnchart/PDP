{-Obras Sociales, Recargado

¡Bienvenidos al hermoso mundo de las obras sociales! Bueno, no es tan hermoso, pero son bienvenidos igual para 
programar el sistema de OSPF (Obra Social de Programadores Funcionales). Veamos algunas nociones: 

De un Socio se sabe su sexo, peso, edad y sus preexistencias (enfermedades diagnosticadas antes de hacerse socio, como 
"estornudoGalopante" y "chiterosisMultiple").
De un Tratamiento se sabe su costo base (es decir, lo que te saldría si tuvieras que atenderte sin obra social), el 
número de sesiones y la enfermedad que trata-}

data Socio = Socio {
    nombre :: String,
    sexo :: String,
    peso :: Float,
    edad:: Int,
    preexistencias :: [String]
} deriving (Show, Eq)


data Tratamiento = Tratamiento {
    costoBase :: Float,
    cantSesiones :: Int,
    enfermedad :: String
} deriving (Show, Eq)

{-Parte A
Modelá a los socios y tratamientos con tuplas, registros (data), listas, o lo que consideres más apropiado, y da 
ejemplos para las siguientes situaciones: 
jose: es hombre, 22 años, 78.9Kg y tiene "zamorreaDistopica"
analia: es mujer, 34 años, 70Kg, y está sana
x1: es tratamiento para la "zamorreaDistopica" sale $5000 y requiere 30 sesiones
xfg23: otro tratamiento para la "zamorreaDistopica", pero sale $10000 y consta de 2 sesiones
Escribí la función diagnosticarPreexistencia, que agrega una preexistencia
Escribí la función estaEnRiesgo, que para un socio es verdadero en los siguientes casos: 
Obesidad (más de 150 Kg)
Edad Avanzada (más de 75 años)
O tiene más de 8 preexistencias
-}

jose = Socio "Jose" "M" 78.9 22 ["zamorreaDistopica"]
analia = Socio "Analia" "F" 70 34 []

x1 = Tratamiento 5000 30 "zamorreaDistopica"
xfg23 = Tratamiento 10000 2 "zamorreaDistopica"

diagnosticarPreexistencia :: [String] -> Socio -> Socio
diagnosticarPreexistencia nuevaPreexistencia socio = socio {preexistencias = preexistencias socio ++ nuevaPreexistencia}

estaEnRiesgo :: Socio -> Bool
estaEnRiesgo socio = (peso socio > 150 && edad socio > 75) || length (preexistencias socio) > 8

{-Parte B
OSPF recibe solicitudes: un pedido de un socio para recibir un tratamiento. Y como es su obligación cubrir un cierto 
monto de los tratamientos solicitados, creó el concepto de prestación: una regla que nos dice cuánto debe cubrir dada 
una solicitud. 

Modelá las solicitudes y da el siguiente ejemplo: 
solicitud897: jose solicitó el tratamiento x1
Modelá las siguientes prestaciones y declará el tipo Prestacion: 
prestacionTotal: cubre el 100% del costo de cualquier solicitud de tratamiento si es para una enfermedad dada, o nada 
en caso contrario
prestacionSinPreexistencias:  cubre el 50% del costo de la solicitud, si el tratamiento es para una enfermedad de la 
que el socio NO tenga preexistencias, o nada en caso contrario
prestacionHastaMaximo: cubre hasta $N pesos del costo para cualquier solicitud
nada: no cubre nada de ninguna solicitud
Finalmente, OSPF maneja el concepto de plan: es una prestación especial que suma a otras prestaciones, en la que OSPF 
cubre tanto como el máximo entre lo que todas cubran.  Sabiendo eso: 
Escribí la función sumarPrestaciones, que tome dos prestaciones y devuelva una nueva prestación que (cuando reciba una 
solicitud) cubra tanto como la mayor de ellas. 
Escribí la función plan que tome una lista de prestaciones y las sume a todas. -}

data Solicitud = Solicitud {
    socio :: Socio,
    tratamiento :: Tratamiento
} deriving (Show, Eq)

estaEnfermoConPreexistencias :: Socio -> Bool
estaEnfermoConPreexistencias socio = not (null (preexistencias (socio)))

solicitud897 :: Solicitud
solicitud897 = Solicitud jose x1

prestacionTotal :: Solicitud -> Solicitud
prestacionTotal solicitud
  | estaEnfermoConPreexistencias (socio solicitud) = solicitud {tratamiento = cubrirTotal (tratamiento solicitud)}
  | otherwise = solicitud

cubrirTotal :: Tratamiento -> Tratamiento
cubrirTotal tratamiento = tratamiento {costoBase = 0}

cubrirLaMitad :: Tratamiento -> Tratamiento
cubrirLaMitad tratamiento = tratamiento {costoBase = costoBase tratamiento / 2}

prestacionSinPreexistencias :: Solicitud -> Solicitud
prestacionSinPreexistencias solicitud 
        | not (estaEnfermoConPreexistencias (socio solicitud)) = solicitud {tratamiento = cubrirLaMitad (tratamiento solicitud)}
        | otherwise = solicitud 

prestacionHastaMaximo :: Solicitud -> Float -> Solicitud
prestacionHastaMaximo solicitud n = solicitud {tratamiento = cubrirNCant (tratamiento solicitud) n}

cubrirNCant :: Tratamiento -> Float -> Tratamiento
cubrirNCant tratamiento n = tratamiento {costoBase = costoBase tratamiento - n}

data Prestacion = Total | SinPreexistencias | HastaMaximo Float | Nada deriving (Show, Eq)

sumarPrestaciones :: Prestacion -> Prestacion -> Prestacion
sumarPrestaciones Total _ = Total
sumarPrestaciones _ Total = Total
sumarPrestaciones SinPreexistencias _ = SinPreexistencias
sumarPrestaciones _ SinPreexistencias = SinPreexistencias
sumarPrestaciones (HastaMaximo n1) (HastaMaximo n2) = HastaMaximo (max n1 n2)
sumarPrestaciones (HastaMaximo n) _ = HastaMaximo n
sumarPrestaciones _ (HastaMaximo n) = HastaMaximo n
sumarPrestaciones _ _ = Nada

plan :: [Prestacion] -> Prestacion
plan [] = Nada
plan prestaciones = foldr1 sumarPrestaciones prestaciones


