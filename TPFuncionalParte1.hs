--Punto 1a--
data Planta = PeaShooter | Repeater | Sunflower | Nut | Parhart | Ancdor deriving (Eq, Show)

puntosVida :: Planta -> Int
puntosVida PeaShooter = 5
puntosVida Repeater = 5
puntosVida Sunflower = 7
puntosVida Nut = 100
puntosVida Parhart = 4
puntosVida Ancdor = 6

soles :: Planta -> Int
soles PeaShooter = 0
soles Repeater = 0
soles Sunflower = 1
soles Nut = 0
soles Parhart = 2
soles Ancdor = 3

especiePlanta :: Planta -> String
especiePlanta PeaShooter = "PeaShooter"
especiePlanta Repeater = "Repeater"
especiePlanta Sunflower = "Sunflower"
especiePlanta Nut = "Nut"
especiePlanta Parhart = "PlantaNueva1"
especiePlanta Ancdor= "PlantaNueva2"

poderAtaque :: Planta -> Int
poderAtaque PeaShooter = 2
poderAtaque Repeater = 4
poderAtaque _ = 0  -- Otras plantas no tienen poder de ataque

--punto 1b--
data Zombie = ZombieBase | BalloonZombie | NewspaperZombie | Gargantuar | ZombieSinAccesorio deriving (Eq, Show)

nombreZombie :: Zombie -> [Char]
nombreZombie ZombieBase = "ZombieBase"
nombreZombie BalloonZombie = "BalloonZombie"
nombreZombie NewspaperZombie = "NewspaperZombie"
nombreZombie Gargantuar = "GargantuarHulkSmashPunyGod"
nombreZombie ZombieSinAccesorio = "ZombieSinAccesorio"

accesorio :: Zombie -> Maybe String
accesorio ZombieBase = Nothing
accesorio BalloonZombie = Just "Globo"
accesorio NewspaperZombie = Just "Diario"
accesorio Gargantuar = Just "Poste eléctrico y zombie enano"
accesorio ZombieSinAccesorio = Nothing

danioMordida :: Zombie -> Int
danioMordida ZombieBase = 1
danioMordida BalloonZombie = 1
danioMordida NewspaperZombie = 2
danioMordida Gargantuar = 30
danioMordida ZombieSinAccesorio = 1

especieZombie :: Zombie -> String
especieZombie ZombieBase = "Zombie Base"
especieZombie BalloonZombie = "Balloon Zombie"
especieZombie NewspaperZombie = "Newspaper Zombie"
especieZombie ZombieSinAccesorio = "Zombie Sin Accesorio"
especieZombie Gargantuar = "Gargantuar Hulk Smash Puny God"

nivelDeMuerte :: Zombie -> Int
nivelDeMuerte zombie = length (nombreZombie zombie)

--Punto 2a--
especialidad :: Planta -> String
especialidad planta
  | poderAtaque planta > puntosVida planta = "Atacante"
  | soles planta > 0 = "Proveedora"
  | poderAtaque planta == 0 = "Defensiva"
  | otherwise = "Desconocida"

--Punto 2b--
esPeligroso :: Zombie -> Bool
esPeligroso zombie = length (nombreZombie zombie) > 10 || length (accesorio zombie) > 1

--Punto 3a--

data LineaDeDefensa = LineaDeDefensa{
    plantas :: [Planta],
    zombies :: [Zombie]
} deriving(Show,Eq);

agregarPlanta :: LineaDeDefensa -> Planta -> LineaDeDefensa
agregarPlanta lineaDeDefensa planta = lineaDeDefensa {plantas = plantas lineaDeDefensa ++ [planta]}

agregarZombie :: LineaDeDefensa -> Zombie -> LineaDeDefensa
agregarZombie lineaDeDefensa zombie = lineaDeDefensa {zombies = zombies lineaDeDefensa ++ [zombie]}

--Punto 3b--
totalAtaque :: [Planta] -> Int
totalAtaque plantas = foldr ((+) . poderAtaque) 0 plantas

totalMordiscos :: [Zombie] -> Int
totalMordiscos zombies = foldr ((+) . danioMordida) 0 zombies

estaEnPeligro :: [Planta] -> [Zombie] -> Bool
estaEnPeligro plantas zombies = totalAtaque plantas < totalMordiscos zombies || (all esPeligroso zombies && not (null zombies))

--Punto 3c--
necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = all (\planta -> especialidad planta == "Proveedora") (plantas linea)

--Punto 4--
lineaMixta :: LineaDeDefensa -> Bool
lineaMixta [] = False;
lineaMixta [_] = False;
lineaMixta (planta1:planta2:ps) = all (/= especialidad planta1) (map especialidad (planta2:ps));

--Punto 5a--
ataquePlanta :: Planta -> Zombie -> String
ataquePlanta planta zombie = drop (poderAtaque planta) (nombreZombie zombie)

--Punto 5b--
--ataqueZombie :: Zombie -> Planta -> Int
--ataqueZombie zombie planta = puntosVida planta - danioMordida zombie
