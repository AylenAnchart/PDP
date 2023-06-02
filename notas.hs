{-Notas
[ Tuplas ]

Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1, nota2), p.ej. un patito en 
el 1ro y un 7 en el 2do se representan mediante el par (2,7).

A partir de esto:
1. Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 4, False en caso contrario. 
No vale usar guardas.
2. Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo.
3. Definir la función promociono, que indica si promocionó, para eso las dos notas tienen que sumar al menos 14 y además 
debe haberse sacado al menos 6 en los dos parciales.
-}
type Notas = (Float, Float)

promedioNotas :: Notas -> Float
promedioNotas (nota1, nota2) = (nota1 + nota2) / 2 -- NO OLVIDAR QUE EN TUPLAS LA VARIABLE SE ESCRIBE TAMBIÉN COMO TUPLA

esNotaBochazo :: Notas -> Bool
esNotaBochazo notas = promedioNotas notas < 4

aprobo :: Notas -> Bool
aprobo = not.esNotaBochazo

promociono :: Notas -> Bool
promociono (nota1, nota2) = nota1 >= 6 && nota2 >= 6 && nota1 + nota2 >= 14

{-Siguiendo con el dominio del ejercicio anterior, tenemos ahora dos parciales con dos recuperatorios, lo representamos 
mediante un par de pares:

((parc1, parc2), (recup1, recup2))

Si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente. Observamos que con 
la codificación elegida, siempre la mejor nota es el máximo entre nota del parcial y nota del recuperatorio. Considerar 
que vale recuperar para promocionar. En este ejercicio vale usar las funciones que se definieron para el anterior.-}
{-}
type Parciales = ((Int, Int), (Int,Int))


rindioRecuperatorioTotal :: ((Int, Int), (Int, Int)) -> Bool
rindioRecuperatorioTotal ((parc1, parc2),(recup1, recup2)) = recup1 /= -1 || recup2 /= -1

rindioRecuperatorio :: ((Int, Int), (Int, Int)) -> (Bool, Bool)
rindioRecuperatorio ((parc1, parc2), (recup1, recup2)) = (recup1 /= -1 && recup2 /= -1) = (True, True) || (recup1 = -1 && recup2 /= -1) = (False, True) || (recup1 /= -1 && recup2 = -1) = (True, False) || (recup1 = -1 && recup2 = -1) = (False, False)


separarParciales :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
separarParciales ((parc1, parc2),(recup1, recup2)) = ((parc1, recup1), (parc2, recup2))

-}
{-4. Definir la función notasFinales que recibe un par de pares y devuelve el par que corresponde a las notas finales 
del alumno para el 1er y el 2do parcial. P.ej.

Main> notasFinales ((2,7),(6,-1))
(6,7)
Main> notasFinales ((2,2),(6,2))
(6,2)
Main> notasFinales ((8,7),(-1,-1))
(8,7)

5. Definir la función recuperoDeGusto que dado el par de pares que representa a un alumno, devuelve True si el alumno,
 pudiendo promocionar con los parciales (o sea sin recup.), igual rindió al menos un recup. Vale definir funciones 
 auxiliares.
-}