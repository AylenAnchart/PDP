-- Punto 1a parte 1--
data Planta = Planta{
    especiePlanta :: String,
    cantPuntosVida :: Int,
    cantSoles :: Int,
    poderDeAtaque :: Int
} deriving(Show,Eq)

peaShooter = Planta "PeaShooter" 5 0 2
repeater = Planta "Repeater" 5 0 4
sunflower = Planta "Sunflower" 7 1 0
nut = Planta "Nut" 100 0 0
parhart = Planta "Parhart" 3 0 7
ancdor = Planta "Ancdor" 11 2 1
cactus = Planta "Cactus" 9 0 0

-- Punto 1b parte 1 --

data Zombie = Zombie{
    especieZombie :: String,
    accesorios :: [String],
    poderDeMordida :: Int
} deriving (Show)

zombieBase = Zombie "ZombieBase" [] 1
balloonZombie = Zombie "BalloonZombie" ["Globo"] 1
newspaperZombie = Zombie "NewspaperZombie" ["Periodico"] 2
gargantuar = Zombie "GargantuarHulkSmashPunyGod" ["Poste electrico","zombie enano"] 30

nivelDeMuerte :: Zombie -> Int
nivelDeMuerte = length.especieZombie

--Punto 2a parte 1--

especialidad :: Planta -> String
especialidad planta
  | poderDeAtaque planta > cantPuntosVida planta = "Atacante"
  | cantSoles planta > 0 = "Proveedora"
  | otherwise = "Defensiva"


--Punto 2b parte 1--


esPeligroso :: Zombie -> Bool
esPeligroso zombie = (not . null . accesorios) zombie || (> 10) (nivelDeMuerte zombie)

--Punto 3a parte 1 --

data LineaDeDefensa = LineaDeDefensa{
    plantas :: [Planta],
    zombies :: [Zombie]
} deriving (Show);

agregarPlanta :: LineaDeDefensa -> Planta -> LineaDeDefensa
agregarPlanta lineaDeDefensa planta = lineaDeDefensa {plantas = plantas lineaDeDefensa ++ [planta]}

agregarZombie :: LineaDeDefensa -> Zombie -> LineaDeDefensa
agregarZombie lineaDeDefensa zombie = lineaDeDefensa {zombies = zombies lineaDeDefensa ++ [zombie]}

--Punto 3b parte 1--

ataqueTotal :: (a -> Int) -> (LineaDeDefensa -> [a]) -> LineaDeDefensa -> Int
ataqueTotal poderDeAtaqueFunc obtenerElementosFunc linea = sum (map poderDeAtaqueFunc (obtenerElementosFunc linea))

ataqueTotalPlantas :: LineaDeDefensa -> Int
ataqueTotalPlantas = ataqueTotal poderDeAtaque plantas

ataqueTotalZombies :: LineaDeDefensa -> Int
ataqueTotalZombies = ataqueTotal poderDeMordida zombies

estaEnPeligro ::  LineaDeDefensa -> Bool
estaEnPeligro linea = ataqueTotalPlantas linea < ataqueTotalZombies linea || all esPeligroso (zombies linea) && not (null(zombies linea));

--Punto 3c parte 1 --

necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida = all ((== "Proveedora") . especialidad) . plantas

--Punto 4 parte 1 --

lineaMixta :: LineaDeDefensa -> Bool
lineaMixta (LineaDeDefensa [] _) = False --Si la lista es vacia
lineaMixta (LineaDeDefensa [_] _) = False --Si la lista solo tiene un elemento
lineaMixta (LineaDeDefensa (p1:p2:ps) _) = all (/= especialidad p1) (map especialidad (p2:ps))

--Punto 5a parte 1 --

ataquePlantaAZombie :: Planta -> Zombie -> Zombie
ataquePlantaAZombie planta zombie
  | especiePlanta planta == "Cactus" && "Globo" `elem` accesorios zombie = zombie { especieZombie = drop (poderDeAtaque planta) (especieZombie zombie), accesorios = filter (/= "Globo") (accesorios zombie) }
  | otherwise = zombie { especieZombie = drop (poderDeAtaque planta) (especieZombie zombie) }
-- drop: primera n cant de elementos, los saca.

--Punto 5b parte 1--

ataqueZombieAPlanta :: Zombie ->  Planta ->  Planta 
ataqueZombieAPlanta zombie planta = planta {cantPuntosVida = max 0 (cantPuntosVida planta- poderDeMordida zombie)}

{- Prueba del punto 4 -}
linea1 = LineaDeDefensa {
    plantas = [peaShooter],
    zombies = []
}

linea2 = LineaDeDefensa {
    plantas = [peaShooter, nut],
    zombies = []
}

linea3 = LineaDeDefensa {
    plantas = [peaShooter, parhart, ancdor],
    zombies = []
}

{- Punto 1 parte 2

i. Si hubiera una cantidad infinita de zombies bases en la misma línea y consultamos si la línea está 
en peligro utilizando la función estaEnPeligro, la evaluación de la función no finalizaría. Esto se debe a 
que la función ataqueTotalPlantas intentaría calcular la suma de los ataques de todas las plantas en la línea, 
mientras que la función ataqueTotalZombies intentaría calcular la suma de los ataques de una cantidad infinita 
de zombies bases. En ambos casos, no se puede completar la suma en una lista infinita, lo que resultaría en una 
evaluación que no finaliza.

ii. Si consultamos si una línea con una cantidad infinita de PeaShooters necesita ser defendida utilizando la función necesitaSerDefendida, 
la evaluación de la función no finalizaría. Esto se debe a que la función "all ((== "Proveedora") . especialidad) . plantas" intentaría 
verificar si todos los elementos de la lista de plantas tienen la especialidad "Proveedora". En el caso de una cantidad infinita de PeaShooters,
la verificación nunca se completaría, ya que se intentaría verificar la especialidad de una cantidad infinita de plantas.

Por otro lado, si consultamos si una línea con una cantidad infinita de Sunflowers necesita ser defendida utilizando la misma función 
necesitaSerDefendida, la evaluación de la función se completaría y devolvería True. Esto se debe a que en la función 
"all ((== "Proveedora") . especialidad) . plantas", la especialidad de todas las plantas se compara con "Proveedora". Dado que la especialidad 
de los Sunflowers es "Proveedora", y se tienen una cantidad infinita de ellos, la verificación para cada planta en la lista se cumpliría y 
la función devolvería True.

iii. En el primer caso, el cálculo de las sumas en una lista infinita de zombies bases nunca se completaría.
En el segundo caso, la verificación de la especialidad en una lista infinita de PeaShooters tampoco se completaría. Sin embargo, en el caso
de una lista infinita de Sunflowers, dado que la verificación se cumple para cada elemento, la función devolvería True debido a la naturaleza
infinita de los Sunflowers en la lista.

-}
-- Punto 3 Parte 2 

jardin :: [LineaDeDefensa]
jardin = [linea1, linea2, linea3]

type Horda = [(Zombie, LineaDeDefensa)]

agregarHorda :: Horda -> [LineaDeDefensa] -> [LineaDeDefensa]
agregarHorda horda jardin = map (\(zombie, linea) -> agregarZombie linea zombie) horda


-- Prueba 

hordaEjemplo :: Horda
hordaEjemplo = [(zombieBase, linea1), (balloonZombie, linea2), (newspaperZombie, linea3)]

nuevoJardin :: [LineaDeDefensa]
nuevoJardin = agregarHorda hordaEjemplo jardin

--Punto 4 Parte 2

rondaDeAtaque :: Planta -> Zombie -> (Planta, Zombie)
rondaDeAtaque planta zombie = (ataqueZombieAPlanta zombie planta, ataquePlantaAZombie planta zombie)

--Punto 5 Parte 2 

plantaMurio :: Planta -> Bool
plantaMurio planta = cantPuntosVida planta <= 0

zombieMurio :: Zombie -> Bool
zombieMurio zombie = nivelDeMuerte zombie <= 0

--Punto 6 Parte 2

fuegoCruzado :: Planta -> Zombie -> (Bool, Bool)
fuegoCruzado planta zombie = (plantaMurio (fst (rondaDeAtaque planta zombie)), zombieMurio (snd (rondaDeAtaque planta zombie)))

--Punto 7 Parte 2 

type Jardin = [LineaDeDefensa]

resultadoAtaques :: Jardin -> Horda -> [(LineaDeDefensa, LineaDeDefensa)]
resultadoAtaques jardin horda = foldl (\acumulador (planta, zombie) -> (ataqueZombieAPlanta zombie planta, ataquePlantaAZombie planta zombie) : acumulador) [] (zip jardin horda)

ataqueRonda :: LineaDeDefensa -> LineaDeDefensa
ataqueRonda linea = lineaNueva (initPlantas (plantas linea)) (tailZombies (zombies linea))

lineaNueva :: [Planta] -> [Zombie] -> LineaDeDefensa
lineaNueva plantas zombies = LineaDeDefensa (initPlantas plantas) (tailZombies zombies)

initPlantas :: [Planta] -> [Planta]
initPlantas lineaDefensa = init lineaDefensa

tailZombies :: [Zombie] -> [Zombie]
tailZombies lineaDefensa = tail lineaDefensa

ataqueSerie :: LineaDeDefensa -> LineaDeDefensa
ataqueSerie linea
  | null (plantas linea) || null (zombies linea) = linea
  | otherwise = ataqueSerie (ataqueRonda linea)

--Punto 8 parte 2

theZombiesAteYourBrains :: Jardin -> [Horda] -> Bool
theZombiesAteYourBrains jardin hordas = all (null . plantas . fst) (concatMap (resultadoAtaques jardin) hordas)

--Punto 9 parte 2

todosZombiesMenosNombres :: Zombie -> LineaDeDefensa -> Bool
todosZombiesMenosNombres zombie linea = all (\z -> length (especieZombie z) < length (especieZombie zombie)) (zombies linea)

{- Punto 10 parte 2 

A- Se verifica si el elemento h está presente en la lista. Se utilizan funciones de orden superior, como filter, para filtrar 
elementos de la lista basados en una condición dada por la función m. La función m se utiliza como argumento en la composición 
con filter, es decir, filter (m h) lista. Se utiliza una tupla p en la cláusula otherwise, y se accede al primer elemento de la 
tupla usando fst p.}
B- Mejora: 
f :: Eq a => a -> (a -> Bool) -> (b, c) -> [a] -> b
f h m p lista
    | h `elem` lista = head (filter (m h) lista)
    | otherwise = fst p
C- Si la lista es infinita, la evaluación de la función puede resultar en un bucle infinito o no finalizar en ciertos casos. 
-}

--Punto 11 parte 2

nivelSupervivencia :: LineaDeDefensa -> Int
nivelSupervivencia linea = sum (map cantPuntosVida (plantas linea)) - sum (map nivelDeMuerte (zombies linea))

