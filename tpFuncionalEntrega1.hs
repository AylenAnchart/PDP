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

zombieBase = Zombie "Zombie Base" [] 1;
balloonZombie = Zombie "Balloon Zombie" " Globo " 1;
newspaperZombie = Zombie "Newspaper Zombie" " Periodico " 2;
gargantuar = Zombie "Gargantuar Hulk Smash Puny God" " Poste Electrico, Zombie Enano " 30;

nivelDeMuerte :: Zombie -> Int
nivelDeMuerte = length.Zombie.especieZombie;

--Punto 2a--

especialidad :: Planta -> String
especialidad planta | cantSoles planta > 0 = "Proveedora";
                    | poderDeAtaque planta > cantPuntosVida planta = "Atacante";
                    | otherwise = "Defensiva";

--Punto 2b--

esPeligroso :: Zombie -> Bool
esPeligroso zombie | length (Zombie.accesorios zombie) > 0;
                   | Zombie.nivelDeMuerte zombie > 10;

--Punto 3a--

data LineaDeDefensa = LineaDeDefensa{

    planta :: [Planta],
    zombie :: [Zombie],
} deriving(Show,Eq);

agregarAlFinalLinea ::  [String] -> [String] -> [String]
agregarAlFinalLinea lista linea = (++).linea.lista;

agregarZombieALinea :: LineaDeDefensa -> [String] -> LineaDeDefensa
agregarZombieALinea = agregarAlFinalLinea.LineaDeDefensa.zombie;

agregarPlantaALinea :: LineaDeDefensa -> [String] -> LineaDeDefensa
agregarPlantaALinea = agregarAlFinalLinea.LineaDeDefensa.planta;

--Punto 3b--

ataqueTotalPlantas :: LineaDeDefensa -> Int
ataqueTotal ld = Sum (map Planta.poderDeAtaque (LineaDeDefensa.planta ld)); --ld es linea de defensa (despues le ponemos algo mas expresivo)

ataqueTotalZombies :: LineaDeDefensa -> Int
ataqueTotal ld = Sum (map Zombie.poderDeMordida (LineaDeDefensa.zombie ld));

estaEnPeligro ::  LineaDeDefensa -> Bool
estaEnPeligro linea | ataqueTotalPlantas linea < ataqueTotalZombies linea
               | all esPeligroso (LineaDeDefensa.zombie linea) && not (null(LineaDeDefensa.zombie linea)) > 0

--Punto 3c--

necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = all esPlantaProveedora (LineaDeDefensa.planta linea)

esPlantaProveedora :: Planta -> Bool
esPlantaProveedora planta = especialidad planta == "Proveedora"
{-
4. Saber si una línea es mixta, que es cuando ninguna de las plantas de la línea tiene
la misma especialidad que su inmediata siguiente. Además, la línea debe tener al
menos dos plantas.
Nota: No usar length (ni ninguna función que tenga el mismo propósito).
> lineaMixta linea2
False
-}

{-
5. Conocer el resultado del ataque de...
a. Una planta a un zombie: Cuando una planta ataca a un zombie, lo daña
según su potencia de ataque, pero ese daño consiste en quitarle al nombre
del zombie las primeras n letras, siendo n la potencia de ataque de la planta.
Cuando el nivel de muerte del zombie llega a cero, es derrotado, pero eso
no se refleja acá.
b. Un zombie a una planta: El zombie muerde a la planta causándole un daño
a la vida equivalente a la fuerza de su mordida.
-}