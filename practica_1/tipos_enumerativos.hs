-- punto 1 

data Dir = Norte | Este | Sur | Oeste
    deriving Show

opuesto :: Dir -> Dir 
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte 
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True 
iguales Este Este   = True
iguales Sur Sur     = True 
iguales Oeste Oeste = True
iguales _ _         = False

-- punto 2

-- creo el tipo DiaDeSemana
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Domingo = True
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True 
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True 
vieneDespues Domingo Lunes = True
vieneDespues _ _ = False

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True  

-- punto 3

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True True = True 
yTambien _ _  = False

{-
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ True = True 
oBien _ _ = False
-}

