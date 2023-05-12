-- Punto 1a--
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

-- Punto 1b--

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

--Punto 2a--

tipoDePlanta :: Planta -> String
tipoDePlanta planta
    | cantSoles planta > 0 = "Proveedora"
    | poderDeAtaque planta > cantPuntosVida planta = "Atacante"
    | otherwise = "Defensiva"

especialidad :: Planta -> String
especialidad planta
  | poderDeAtaque planta > cantPuntosVida planta = "Atacante"
  | cantSoles planta > 0 = "Proveedora"
  | otherwise = "Defensiva"


--Punto 2b--

esPeligroso :: Zombie -> Bool
esPeligroso zombie = length (accesorios zombie) > 0 || nivelDeMuerte zombie > 10

--Punto 3a--

data LineaDeDefensa = LineaDeDefensa{
    plantas :: [Planta],
    zombies :: [Zombie]
} deriving (Show);

agregarPlanta :: LineaDeDefensa -> Planta -> LineaDeDefensa
agregarPlanta lineaDeDefensa planta = lineaDeDefensa {plantas = plantas lineaDeDefensa ++ [planta]}

agregarZombie :: LineaDeDefensa -> Zombie -> LineaDeDefensa
agregarZombie lineaDeDefensa zombie = lineaDeDefensa {zombies = zombies lineaDeDefensa ++ [zombie]}

--Punto 3b--

ataqueTotalPlantas :: LineaDeDefensa -> Int
ataqueTotalPlantas linea = sum (map poderDeAtaque (plantas linea)); 

ataqueTotalZombies :: LineaDeDefensa -> Int
ataqueTotalZombies linea = sum (map poderDeMordida (zombies linea));

estaEnPeligro ::  LineaDeDefensa -> Bool
estaEnPeligro linea = ataqueTotalPlantas linea < ataqueTotalZombies linea || all esPeligroso (zombies linea) && not (null(zombies linea));

--Punto 3c--

necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = all ((== "Proveedora").especialidad) (plantas linea) 

--Punto 4--

lineaMixta :: LineaDeDefensa -> Bool
lineaMixta (LineaDeDefensa [] _) = False --Si la lista es vacia
lineaMixta (LineaDeDefensa [_] _) = False --Si la lista solo tiene un elemento
lineaMixta (LineaDeDefensa (p1:p2:ps) _) = all (/= especialidad p1) (map especialidad (p2:ps))

--Punto 5a--

ataquePlantaAZombie :: Planta -> Zombie -> Zombie
ataquePlantaAZombie  planta zombie = zombie {especieZombie =  drop (poderDeAtaque planta) (especieZombie zombie)}
-- drop: primera n cant de elementos, los saca.

--Punto 5b--

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