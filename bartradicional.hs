import Distribution.Compat.Lens (_1)
{-Bar Tradicional
[Tuplas, Tipos Propios y sinónimos de tipos]

En un tradicional bar de la ciudad nos piden un sistema que los ayude en su trabajo diario.

Durante un relevamiento obtuvimos la siguiente información:
-}
type Nombre = String
type Agregado = String
type Sabor = String
type Azucar = Integer
type Sabores = (String, String)
type Baño = String

data Producto = Cafe Nombre Azucar 
            | Gaseosa Sabor Azucar 
            |Helado Sabores Baño 
            | AguaMineral deriving Show


{-}
Definir aprovechando los conceptos del paradigma funcional, las siguientes funciones.

1) Dada un producto determinar la cantidad de calorías que contiene
Si es café la cantidad de calorías es 3 veces la cantidad de azúcar que contiene.
Si es gaseosa  la cantidad de calorías es igual a la cantidad de azúcar.
En cuanto a los helados cualquier helado tiene 220, pero si además tiene baño de “chocolate”
 y alguno de sus sabores es “chocolate” se le suma 150 calorías más.
El agua mineral no tiene calorías.

Main> calorias (Helado (“cereza”, “chocolate”) “chocolate”)
370-}

calorias :: Producto -> Integer
calorias (Cafe _ azucar) = 3 * azucar
calorias (Gaseosa _ azucar) = azucar
calorias (Helado sabores baño)
  | baño == "chocolate" && ("chocolate" `elem` sabores) = 370
  | otherwise = 220
calorias AguaMineral = 0

{-}
2) Dado un producto determinar si el contenido calórico es bajo, medio o alto.
Si tiene menos de 100 calorías es bajo.
Si tiene entre 100 y 200 es medio.
Si supera los 200 es alto.

Main> contenidoCalorico AguaMineral
“bajo”-}

contenidoCalorico :: Producto -> String
contenidoCalorico producto | calorias producto < 100 = "bajo"
                  | calorias producto <= 200 = "medio"
                  | calorias producto > 200 = "alto"

{-}
3) Dado un producto determinar si es energizante.
Si es café es energizante si es un capuchino.
Si es una gaseosa es energizante si es de sabor a pomelo.
En caso de ser helado es energizante si alguno de los sabores es chocolate.

Main> esEnergizante (Gaseosa “pomelo” 30)
True-}

capuchino = Cafe "capuchino" 10
pomelo = Gaseosa "pomelo" 10
chocolate = Helado ("chocolate", "limon") "chocolate"

esEnergizante :: Producto -> Bool
esEnergizante (Cafe nombre _) = nombre == "capuchino"
esEnergizante (Gaseosa sabor _) = sabor == "pomelo"
esEnergizante (Helado (sabor1, sabor2) _) = sabor1 == "chocolate" || sabor2 == "chocolate"

{-}
4) Determinar si un producto es recomendable esto es si cumple alguna de las condiciones:
Es energizante
Tiene menos de 100 calorías

Main> esRecomendable (Cafe “capuchino” 40)
True-}

esRecomendable :: Producto -> Bool
esRecomendable producto = calorias producto < 100 && esEnergizante producto

{-
5) Dado 2 sabores y un baño, devolver el helado de dichos sabes y con dicho baño.
Main> preparar “cereza” “melon”  “chocolate”
Helado (“cereza”, “melon”)  “chocolate”-}

preparar :: String -> String -> String -> Producto
preparar sabor1 sabor2 bano = Helado (sabor1, sabor2) bano
