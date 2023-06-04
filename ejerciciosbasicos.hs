{-Notas previas
En algunos ejercicios se van a utilizar algunas de las funciones que están en el Prelude por ej: 
mod 20 3 = 2	el resto de la división entre 20 y 3 es 2. 
div 14 3 = 4	parte entera de la división entre 14 y 3 es 4. 
max 8 10 = 10	devuelve el max entre 2 números. 
min 9 15 = 9	devuelve el min entre 2 números. 

-}

{-Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3, p.ej: 
Main> esMultiploDeTres 9 
True-}

esMultiploDeTres :: Int -> Bool
esMultiploDeTres nro = mod nro 3 == 0

{- Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero, p.ej. 
Main> esMultiploDe 3 12
True-}

esMultiplo:: Int -> Int -> Bool
esMultiplo nro1 nro2 = mod nro2 nro1 == 0

cubo :: Int -> Int
cubo nro = nro * nro * nro

{-Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.-}

areaDelRectangulo :: Int -> Int -> Int
areaDelRectangulo base altura = base * altura 

{-Definir la función esBisiesto/1, indica si un año es bisiesto. (Un año es bisiesto si es divisible por 400 o es 
divisible por 4 pero no es divisible por 100) Nota: Resolverlo reutilizando la función esMultiploDe/2-}

esBisiesto :: Int -> Bool
esBisiesto anio = (esMultiplo anio 400 || esMultiplo anio 4) && not (esMultiplo anio 100)

{-Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.-}

celsiusToFahr :: Float -> Float
celsiusToFahr temp =  (1.8 * temp) + 32

{-Definir la función fahrToCelsius/1, la inversa de la anterior.-}

fahrToCelsius :: Float -> Float
fahrToCelsius temp = (0.5 * temp) - 32

{-Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace 
frío si la temperatura es menor a 8 grados Celsius.-}

haceFrio :: Float -> Bool
haceFrio temp = (fahrToCelsius temp) < 8

{-Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula. 
m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)} 
Más información. 
Nota: Se puede utilizar gcd.-}

mcm :: Int -> Int -> Int
mcm nro1 nro2 = nro1 * nro2

{-Dispersión
Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de Corrientes medido en tres días 
consecutivos; cada medición es un entero que representa una cantidad de cm. 
P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm. 
A partir de estos tres números, podemos obtener algunas conclusiones. 
Definir estas funciones: 

dispersion, que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. Ayuda: extender max y 
min a tres argumentos, usando las versiones de dos elementos. De esa forma se puede definir dispersión sin escribir 
ninguna guarda (las guardas están en max y min, que estamos usando). 

diasParejos, diasLocos y diasNormales reciben los valores de los tres días. Se dice que son días parejos si la dispersión
 es chica, que son días locos si la dispersión es grande, y que son días normales si no son ni parejos ni locos. Una 
 dispersión se considera chica si es de menos de 30 cm, y grande si es de más de un metro. 
Nota: Definir diasNormales a partir de las otras dos, no volver a hacer las cuentas. -}

dispersion :: Int -> Int -> Int -> Int
dispersion nro1 nro2 nro3 = max nro1 (max nro2 nro3) - min nro1 (min nro2 nro3)

diasParejos :: Int -> Int -> Int -> Bool
diasParejos nro1 nro2 nro3 = dispersion nro1 nro2 nro3 < 30

diasLocos :: Int -> Int -> Int -> Bool
diasLocos nro1 nro2 nro3 = dispersion nro1 nro2 nro3 > 100

diasNormales :: Int -> Int -> Int -> Bool
diasNormales nro1 nro2 nro3 = not (diasParejos nro1 nro2 nro3 && diasLocos nro1 nro2 nro3)

{-En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. El peso de un pino se puede calcular 
a partir de la altura así: 3 kg x cm hasta 3 metros, 2 kg x cm arriba de los 3 metros. P.ej. 2 metros ⇒  600 kg, 
5 metros ⇒  1300 kg. 
Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, un 
pino fuera de este rango no le sirve a la fábrica. Para esta situación: 
Definir la función pesoPino, recibe la altura de un pino y devuelve su peso. 
Definir la función esPesoUtil, recibe un peso en kg y devuelve True si un pino de ese peso le sirve a la fábrica, y 
False en caso contrario. 
Definir la función sirvePino, recibe la altura de un pino y devuelve True si un pino de ese peso le sirve a la fábrica, 
y False en caso contrario. Usar composición en la definición. -}


pesoPino :: Int -> Int
pesoPino altura | altura <= 3 = altura * 3 |
                 altura > 3 = altura * 2

esPesoUtil :: Int -> Bool
esPesoUtil peso  = peso >= 400 && peso <= 1000

sirvePino :: Int -> Bool 
sirvePino = esPesoUtil.pesoPino {- recibe la altura = 500 (se multiplica por 2 y queda el peso en 1000, por lo que es util)-}

{-Este ejercicio alguna vez se planteó como un Desafío Café con Leche: Implementar la función esCuadradoPerfecto/1, 
sin hacer operaciones con punto flotante. Ayuda: les va a venir bien una función auxiliar, tal vez de dos parámetros. 
Pensar que el primer cuadrado perfecto es 0, para llegar al 2do (1) sumo 1, para llegar al 3ro (4) sumo 3, para llegar 
al siguiente (9) sumo 5, después sumo 7, 9, 11 etc.. También algo de recursividad van a tener que usar. -}

esCuadradoPerfecto :: Int -> Bool
esCuadradoPerfecto n = cuadradoPerfectoAux n 0 1

cuadradoPerfectoAux :: Int -> Int -> Int -> Bool
cuadradoPerfectoAux n suma impar
  | suma == n = True -- El número es un cuadrado perfecto
  | suma > n = False -- El número no es un cuadrado perfecto
  | otherwise = cuadradoPerfectoAux n (suma + impar) (impar + 2)








