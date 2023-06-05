data Ciudadano = Ciudadano{
profesion::String,
sueldo::Int,
cantidadHijos::Int,
bienes::[Bienes]
}
type Bienes= (String, Int)

homero = Ciudadano "Seguridad Nuclear" 9000 3 [("Casa",5000),("deuda",70000)]
frink = Ciudadano "Profesor" 12000 1 []
burns = Ciudadano "Empresario" 30000 1 [("Empresa",100000),("Empresa",50000),("deuda",70000)]
krabappel = Ciudadano "profesor" 12000 0[("Casa",35000)]

type Ciudad = [Ciudadano]
springfield = [homero, burns, frink, krabappel]

{-diferenciaDePatrimonio: recibe una ciudad y dice cuál es la
diferencia entre el ciudadano que más patrimonio tiene y el que
menos patrimonio tiene. El patrimonio de cada ciudadano se
obtiene sumando el valor de su sueldo y de sus bienes-}

diferenciaDePatrimonio :: Ciudad -> Int
diferenciaDePatrimonio unaCiudad = patrimonioMasTiene unaCiudad - patrimonioMenosTiene unaCiudad

patrimonioMasTiene :: Ciudad -> Int
patrimonioMasTiene unaCiudad = maximum (map patrimonioTotal unaCiudad)

patrimonioMenosTiene :: Ciudad -> Int
patrimonioMenosTiene unaCiudad = minimum (map patrimonioTotal unaCiudad)

patrimonioTotal :: Ciudadano -> Int
patrimonioTotal unCiudadano = sueldo unCiudadano + valores unCiudadano

valores :: Ciudadano -> Int
valores unCiudadano = foldl (\bienes (_, valor)-> bienes + valor) 0 (bienes unCiudadano)

{-tieneAutoAltaGama: recibe un ciudadano y dice si tiene un auto de alta gama, ó sea, si tiene entre
sus bienes un auto que valga más de 100000.
tieneAutoAltaGama unCiudadano = any (>1000-}

tieneAutoAltaGama :: Ciudadano -> Bool
tieneAutoAltaGama unCiudadano =  any((> 100000) . snd) (filter ((== "Auto") . fst) (bienes unCiudadano))

{-Medidas: se aplican a un ciudadano, y lo retornan modificado.
a. auh: Hace que aumente el sueldo de la persona en 1000 por cada hijo, si el patrimonio de la
persona es menor a 0 (en otro caso, el ciudadano no cambia).
-}

auh :: Ciudadano -> Ciudadano
auh unCiudadano | patrimonioTotal unCiudadano < 0 = unCiudadano {sueldo = sueldo unCiudadano + (1000 * (cantidadHijos unCiudadano))}
                | otherwise = unCiudadano

{-impuestoGanancias: si el sueldo supera el mínimo dado , disminuye su sueldo el 30% de la
diferencia. Si no supera el mínimo, queda igual.-}

impuestoGanancias :: Int -> Ciudadano -> Ciudadano
impuestoGanancias numero unCiudadano | sueldo unCiudadano > numero = unCiudadano {sueldo = sueldo unCiudadano - (sueldo unCiudadano * 3)}
                                     | otherwise = unCiudadano

{-impuestoAltaGama: si el ciudadano tiene algún auto de alta gama, disminuye su sueldo en un
10% del valor del auto, sino no disminuye nada.
-}

impuestoaltaGama:: Ciudadano -> Ciudadano
impuestoaltaGama unCiudadano | tieneAutoAltaGama unCiudadano = unCiudadano {sueldo=sueldo unCiudadano - porcValorDelAuto unCiudadano}
                             | otherwise = unCiudadano



