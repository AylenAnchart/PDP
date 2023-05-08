-- Punto 1a--
data Planta = Planta{
    especiePlanta :: String,
    cantPuntosVida :: Int,
    cantSoles :: Int,
    poderDeAtaque :: Int
} deriving(Show,Eq);

peaShooter = Planta "PeaShooter" 5 0 2;
repeater = Planta "Repeater" 5 0 4;
sunflower = Planta "Sunflower" 7 1 0;
nut = Planta "Nut" 100 0 0;
parhart = Planta "Parhart" 3 0 7;  
ancdor = Planta "Ancdor" 11 2 1;

-- Punto 1b--

data Zombie = Zombie{
    especieZombie :: String,
    accesorios :: [String],
    poderDeMordida :: Int
} deriving(Show,Eq);


zombieBase = Zombie "ZombieBase" [] 1;
balloonZombie = Zombie "BalloonZombie" " Globo " 1;
newspaperZombie = Zombie "NewspaperZombie" " Periodico " 2;
gargantuar = Zombie "GargantuarHulkSmashPunyGod" " Poste Electrico, Zombie Enano " 30;

-- Se debe poder determinar su nivel de muerte. El mismo equivale a la cantidad de letras que tiene su nombre
nivelDeMuerte :: Zombie -> Int
nivelDeMuerte = length.especieZombie; -- No se coloca Zombie. porque en el tipado ya especificamos que estamos trabajando en uno

--Punto 2a--

--Determinar de qué especialidad es una planta
tipoDePlanta :: Planta -> String
tipoDePlanta planta
    | cantSoles planta > 0 = "Proveedora"
    | poderDeAtaque planta > cantPuntosVida planta = "Atacante"
    | otherwise = "Defensiva"

especialidad :: Planta -> String
especialidad = tipoDePlanta; --Genere esta funcion para poder utilizar los strings que devolvia la funcio tipoDePlanta

--Punto 2b--

--Determinar si un zombie es peligroso. Esto ocurre cuando tiene más de un accesorio o su nivel de muerte es mayor a 10
esPeligroso :: Zombie -> Bool
esPeligroso zombie = length (accesorios zombie) > 0 || nivelDeMuerte zombie > 10;

--Punto 3a--

data LineaDeDefensa = LineaDeDefensa{
    plantas :: [Planta],
    zombies :: [Zombie]
} deriving(Show,Eq);

agregarALinea :: a -> (LineaDeDefensa -> [a]) -> LineaDeDefensa -> LineaDeDefensa
agregarALinea elemento obtenerLinea linea = linea { obtenerLinea = (obtenerLinea linea) ++ [elemento] }; -- a es el tipo del elemento que queremos agregar puede ser tanto planta como zombie
--  Lo que esta dentro de las llaves {} es para generar una nueva LineaDeDefensa, modificando solamente los atributos que nosotros queremos

--Agregar una planta a una línea (se agrega al final)
agregarZombie:: Zombie -> LineaDeDefensa -> LineaDeDefensa
agregarZombie zombie = agregarALinea zombie zombies;

--Agregar un zombie a una línea (se agrega al final)
agregarPlanta :: Planta -> LineaDeDefensa -> LineaDeDefensa
agregarPlanta planta = agregarALinea planta plantas;

--Punto 3b--

--el total de ataque de todas las plantas es inferior al total de mordiscos de todos los zombies
ataqueTotalPlantas :: LineaDeDefensa -> Int
ataqueTotalPlantas linea = sum (map Planta.poderDeAtaque (plantas linea)); -- te cambie los ld por linea

--todos los zombies de esa línea son peligrosos y hay al menos un zombie
ataqueTotalZombies :: LineaDeDefensa -> Int
ataqueTotalZombies linea = sum (map Zombie.poderDeMordida (zombies linea)); -- te cambie los ld por linea

--Saber si una línea está en peligro
estaEnPeligro ::  LineaDeDefensa -> Bool
estaEnPeligro linea = ataqueTotalPlantas linea < ataqueTotalZombies linea || all esPeligroso (zombies linea) && not (null(zombies linea)) > 0;

--Punto 3c--

-- Poder determinar si una línea necesita ser defendida, esto pasa cuando todas las plantas de esa línea son proveedoras. 
necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = all ((== "Proveedora").especialidad) (plantas linea) 
-- all toma un boleano y una lista entonce ((== "Proveedora").especialidad) es el bool y (plantas linea) la lista(plantas linea) 

--Punto 4--

{-Saber si una línea es mixta, que es cuando ninguna de las plantas de la línea tiene
la misma especialidad que su inmediata siguiente. Además, la línea debe tener al
menos dos plantas.-}
lineaMixta :: LineaDeDefensa -> Bool
lineaMixta (LineaDeDefensa [] _) = False --Si la lista es vacia
lineaMixta (LineaDeDefensa [_] _) = False --Si la lista solo tiene un elemento
lineaMixta (LineaDeDefensa (p1:p2:ps) _) = all (/= especialidad p1) (map especialidad (p2:ps))--Verificamos que si hay dos plantas iguales seguidas, la línea no será mixta.
--Le puse p por posicion, si se te ocurre algo mejor ponelo, la profe usaba x, pero a mi no me gusta.


--Punto 5a--

{-
Cuando una planta ataca a un zombie, lo daña según su potencia de ataque,
pero ese daño consiste en quitarle al nombre del zombie las primeras n letras,
siendo n la potencia de ataque de la planta.
Cuando el nivel de muerte del zombie llega a cero, es derrotado, pero eso no se refleja acá.
-}
ataquePlantaAZombie :: Planta -> Zombie -> Zombie
ataquePlantaAZombie  planta zombie = zombie {especieZombie =  take  Planta.poderDeAtaque planta (Zombie.especieZombie zombie)};
--No tome en cuenta lo de nivel de muerte, aca no afecta pero si se te ocurre generalo.

--Punto 5b--

--Cuando un zombie ataca a una planta: El zombie muerde a la planta causándole un daño a la vida equivalente a la fuerza de su mordida.
ataqueZombieAPlanta :: Zombie ->  Planta ->  Planta 
ataqueZombieAPlanta zombie planta = planta {cantPuntosVida = max 0 (Planta.cantPuntosVida planta- Zombie.poderDeMordida zombie)};
