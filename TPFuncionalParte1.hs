--Punto 1a--
data Planta = PeaShooter | Repeater | Sunflower | Nut | Planta1 | Planta2 deriving (Eq, Show)

puntosVida :: Planta -> Int
puntosVida PeaShooter = 5
puntosVida Repeater = 5
puntosVida Sunflower = 7
puntosVida Nut = 100
puntosVida Planta1 = 4
puntosVida Planta2 = 6

soles :: Planta -> Int
soles PeaShooter = 0
soles Repeater = 0
soles Sunflower = 1
soles Nut = 0
soles Planta1 = 2
soles Planta2 = 3

especiePlanta :: Planta -> String
especiePlanta PeaShooter = "PeaShooter"
especiePlanta Repeater = "Repeater"
especiePlanta Sunflower = "Sunflower"
especiePlanta Nut = "Nut"
especiePlanta Planta1 = "PlantaNueva1"
especiePlanta Planta2 = "PlantaNueva2"

poderAtaque :: Planta -> Int
poderAtaque PeaShooter = 2
poderAtaque Repeater = 4
poderAtaque _ = 0  -- Otras plantas no tienen poder de ataque

--punto 1b--
data Zombie = ZombieBase | BalloonZombie | NewspaperZombie | Gargantuar | ZombieSinAccesorio deriving (Eq, Show)

nombreZombie :: Zombie -> [Char]
nombreZombie ZombieBase = "Zombie Base"
nombreZombie BalloonZombie = "Balloon Zombie"
nombreZombie NewspaperZombie = "Newspaper Zombie"
nombreZombie Gargantuar = "Gargantuar Hulk Smash Puny God"
nombreZombie ZombieSinAccesorio = "Zombie Sin Accesorio"

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
esPeligroso zombie 
  | length (nombreZombie zombie) > 10 = True
  | length (accesorio zombie) > 1 = True

--Punto 3a--
type LineaDeDefensaP = [Planta]
type LineaDeDefensaZ = [Zombie]

agregarPlanta :: Planta -> LineaDeDefensaP -> LineaDeDefensaP
agregarPlanta planta lineaDefensa = lineaDefensa ++ [planta]

agregarZombie :: Zombie -> LineaDeDefensaZ -> LineaDeDefensaZ
agregarZombie zombie lineaDeDefensa = lineaDeDefensa ++ [zombie]

--Punto 3b--
totalAtaque :: LineaDeDefensaP -> Int
totalAtaque [] = 0
totalAtaque plantas = foldr ((+) . poderAtaque) 0 plantas

totalMordiscos :: LineaDeDefensaZ -> Int
totalMordiscos [] = 0
totalMordiscos zombies = foldr ((+) . danioMordida) 0 zombies

estaEnPeligro :: LineaDeDefensaP -> LineaDeDefensaZ -> Bool
estaEnPeligro plantas zombies = totalAtaque plantas < totalMordiscos zombies || (all esPeligroso zombies && not (null zombies))

--Punto 3c--
necesitaSerDefendida :: LineaDeDefensaP -> Bool
necesitaSerDefendida = all (\ planta -> especialidad planta == "Proveedora")

--Punto 4--
lineaMixta :: LineaDeDefensaP -> Bool
lineaMixta [] = False 
lineaMixta [_] = False 
lineaMixta (p1:p2:ps)= all (/= especialidad p1) (map especialidad (p2:ps))

--Punto 5a--
ataquePlanta :: Planta -> Zombie -> String
ataquePlanta planta zombie = drop (poderAtaque planta) (nombreZombie zombie)

--Punto 5b--
ataqueZombie :: Zombie -> Planta -> Int
ataqueZombie zombie planta = puntosVida planta - danioMordida zombie
