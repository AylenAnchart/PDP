data Hombre = UnHombre String Float deriving (Show, Eq)
data Comida = UnaComida Float deriving (Show, Eq)

rosquilla1= UnaComida 300
rosquilla2= UnaComida 200
rosquilla3= UnaComida 150

comer (UnHombre nombre peso) 


panzada :: UnHombre -> UnaComida -> UnHombre
panzada unHombre comidas = foldl comer unHombre comidas

rosquillaGrande comidas = foldl max 0 