{-Listas
Existen funcione predefinidas en el Prelude que nos permiten manejar listas por ej: head l / tail l / l !! i 
(infija, devuelve el elemento en la posición i, base 0). Ejemplos 
head [2,4,6,8] = 2 
tail [2,4,6,8] = [4,6,8] 
[2,4,6,8] !! 1 = 4 	-- base 0!! 
null [] = True 	--Indica si una lista esta vacía. 
null [2,4,5] = False 
concat [[1..4],[11..13],[21,34]] = [1,2,3,4,11,12,13,21,34] -- ”aplana” una lista de listas. 

Si se quiere calcular el promedio, dada una lista números y se está usando prelude puro, se puede hacer algo así: 
> sum [3,5,6] / fromInteger(toInteger(length[3,5,6])) 
4.66666666666667 
Tener en cuenta que en la notación [a..b] ni a ni b tienen por qué ser constantes, pueden ser cualquier expresión, p.ej 
[1..head [6,3,8]] 		[min (3+4) (3*4)..max (3+4) (3*4)] 

Ejercicio Extra
Para cada función que definan, obtengan su tipo y después verifiquen con lo que les dice el :t. ¡Pero háganlo primero 
a mano! si no, no practican. Hacer lo mismo con las funciones head, tail y !!. Para preguntar por el tipo de una función
infija, la ponen entre paréntesis, p.ej. :t (!!) 
-}

{-Ejercicios
 Definir una función que sume una lista de números. 
Nota: Investigar sum 
-}

suma :: [Float] -> Float
suma = foldr (+) 0 -- combino los valores de la lista sumandolos y que se inicialicen en 0 !!

{-Durante un entrenamiento físico de una hora, cada 10 minutos de entrenamiento se tomóo la frecuencia cardíaca de uno 
de los participantes obteniéndose un total de 7 muestras que son las siguientes:
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125] 
Comienza con un frecuencia de 80 min 0. 
A los 10 min la frecuencia alcanza los 100 
A los 20 min la frecuencia es de 120, 
A los 30 min la frecuencia es de 128
A los 40 min la frecuencia es de 130, …etc.. 
A los 60 min la frecuencia es de 125 frecuenciaCardiaca es un función constante. 
Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la frecuencia cardíaca. 
Main> promedioFrecuenciaCardiaca 
115.285714285714
Definir la función frecuenciaCardiacaMinuto/1, que recibe m que es el minuto en el cual quiero conocer la frecuencia 
cardíaca, m puede ser a los 10, 20, 30 ,40,..hasta 60.
Main> frecuenciaCardiacaMomento 30 
128 
Ayuda: Vale definir una función auxiliar para conocer el número de muestra. 
Definir la función frecuenciasHastaMomento/1, devuelve el total de frecuencias que se obtuvieron hasta el minuto m. 
Main> frecuenciasHastaMomento 30 
[80, 100, 120, 128] 
Ayuda: Utilizar la función take y la función auxiliar definida en el punto anterior. -}

newtype Estudiante = Estudiante {estudiante :: [(Min, Frecuencia)]} deriving (Show, Eq)
    
type Frecuencia = [Int]
type Min = [Int]

juan = [(0,80),(10,100),(20,120),(30,128),(40,130),(50,123),(60,125)]

{-Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la frecuencia cardíaca. 
Main> promedioFrecuenciaCardiaca 
115.285714285714-}

promedioFrecuenciaCardiaca :: Frecuencia -> Int
promedioFrecuenciaCardiaca lista = div (sum lista) (length lista)

{-Definir la función frecuenciaCardiacaMinuto/1, que recibe m que es el minuto en el cual quiero conocer la frecuencia 
cardíaca, m puede ser a los 10, 20, 30 ,40,..hasta 60.-}
 
frecuenciaCardiacaMinuto :: Int -> Int
frecuenciaCardiacaMinuto m = (snd.head) (filter ((==m).fst) juan)

{-Definir la función frecuenciasHastaMomento/1, devuelve el total de frecuencias que se obtuvieron hasta el minuto m. 
Main> frecuenciasHastaMomento 30 
[80, 100, 120, 128] 
Ayuda: Utilizar la función take y la función auxiliar definida en el punto anterior. -}


