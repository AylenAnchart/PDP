{-Twitter (#Haskell)
[ Listas, Orden Superior]
En una aplicación que se conecta a Twitter, trabajamos con Tweets, de los que conocemos el usuario que lo escribió, y 
el contenido del tweet (lo que escribió). Se nos pide:
Dado un conjunto de tweets, saber:
la longitud promedio de los mismos.
la cantidad de tweets cortos. Un tweet es corto cuando la longitud de su contenido es menor a 50 caracteres.  

Dado un conjunto de tweets y un usuario, obtener el contenido de los tweets que pertenecen a ese usuario. 
-}
data Tweets = Tweets {
  usuario :: String,
  contenido :: [String]
}

longitudDelTweet :: Tweets -> [Int]
longitudDelTweet tweets = map length (contenido tweets)

tweetCorto :: Tweets -> [String]
tweetCorto tweets = filter (\contenido -> length contenido < 50) (contenido tweets)

cantidadDeTweetCorto :: Tweets -> Int
cantidadDeTweetCorto tweets = length (tweetCorto tweets)

juan :: Tweets
juan = Tweets "juan" ["Hola", "Me aburro", "12345678901234567890123456789012345678901234567890123456789012345"]

pertenecenAlUsuario :: Tweets -> [String]
pertenecenAlUsuario = contenido

{- a. Definir y dar el tipo de la función contar, que toma un criterio, un contador y un elemento, y devuelve el contador + 1 si lo cumple, o el contador si no lo cumple. ej:
  > contar even 10 4
    11  -- 4 es par, por eso devuelve 10 + 1 
-}

contar :: (a -> Bool) -> Int -> a -> Int
contar criterio contador elemento
  | criterio elemento = contador + 1
  | otherwise = contador





