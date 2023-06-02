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