-- Numeros Enteros
-- punto 1

sucesor :: Int -> Int
sucesor n = n+1

sumar :: Int -> Int -> Int
sumar n m = n+m

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = ( div n  m , mod n m )

maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if (n > m)
                        then n
                        else m

-- punto 2

{-
De 4 ejemplos de expresiones diferentes que denoten el número 10, 
utilizando en cada expresión a todas las funciones del punto anterior.
1. maxDelPar (divisionYResto (sucesor 19) (sumar 1 1) )
2. maxDelPar (divisionYResto (sumar 15 5) (sucesor 1) )
3. maxDelPar (divisionYResto (sumar 1 9) (sucesor 0 ) )
4. maxDelPar (divisionYResto (sumar 3 7 ) (sucesor 23 ) )
-}

-- Tipos enumerativos

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

siguiente :: Dir -> Dir
--precond: No se puede pedir Siguiente direccion de Oeste, ya que no existe.
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente _ = error "no existe siguiente direccion"

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
vieneDespues d1 d2 = numeroDia d1 > numeroDia d2

numeroDia :: DiaDeSemana -> Int
numeroDia Lunes     = 1
numeroDia Martes    = 2
numeroDia Miercoles = 3
numeroDia Jueves    = 4
numeroDia Viernes   = 5
numeroDia Sabado    = 6
numeroDia Domingo   = 7

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True  

-- punto 3

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True q = q
implica False _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True x = x
yTambien False _ = False

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ b    = b

--Registros

-- punto 1

data Persona = P String Int 
            -- Nombre Edad 
    deriving Show

lautaro = P "lautaro" 21

nombre :: Persona -> String 
nombre (P n e ) = n

edad :: Persona -> Int
edad (P n e ) = e

crecer :: Persona -> Persona 
crecer (P n e )= P n (e+ 1) 

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (P n e ) = (P nuevoNombre e )

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P nX eX ) (P nY eY ) = eX > eY

laQueEsMayor :: Persona -> Persona -> Persona
-- precon: debe haber una persona mayor a la otra para su correcto funcionamiento
laQueEsMayor p1 p2 = if (edad p1 > edad p2 )
                            then p1
                            else p2

-- punto 2

data Pokemon = Pok TipoDePokemon Int
            -- Tipo PorcentajeDeEnergia 
    deriving Show

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show


data Entrenador = E String Pokemon Pokemon
             -- Nombre Pokemon1 Pokemon2
    deriving Show

charizard = Pok Fuego 10
venasaur = Pok Planta 5
entrenador = E "lautaro" charizard venasaur

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esTipoSuperior (tipoDelPokemon p1) (tipoDelPokemon p2)

tipoDelPokemon :: Pokemon -> TipoDePokemon
tipoDelPokemon (Pok t _) = t

-- indica si el primer tipo de pokemon es superior al segundo
esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua  = True
esTipoSuperior _ _ = False


cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t e = unoSiCeroSino (esDeTipo (tipoDelPokemon (primerPokemonDe e) ) t) + 
                                    unoSiCeroSino (esDeTipo  (tipoDelPokemon (segundoPokemonDe e) ) t)

primerPokemonDe :: Entrenador -> Pokemon
primerPokemonDe (E _ p _ ) = p

segundoPokemonDe :: Entrenador -> Pokemon
segundoPokemonDe (E _ _ p ) = p

unoSiCeroSino:: Bool -> Int
unoSiCeroSino b = if(b)
                    then 1
                    else 0
                    
esDeTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDeTipo Agua Agua   = True
esDeTipo Fuego Fuego = True
esDeTipo Planta Planta = True
esDeTipo _ _ = False

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = pokemonsDe e1 ++ pokemonsDe e2

pokemonsDe :: Entrenador -> [Pokemon]
pokemonsDe (E _ p1 p2) = [p1, p2]

--Funciones polimorficas

-- punto 1

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)

{-
punto 2 
Estas funciones son polimorficas porque tienen sentido para más de un tipo
-}


-- Pattern Matching sobre listas

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
-- Precond: La lista no debe ser vacia
elPrimero [] = error "la lista no puede ser vacia"
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
-- Precond: La lista no debe ser vacia
sinElPrimero [] = error "la lista no puede ser vacia"
sinElPrimero (_:xs) = xs

splitHead :: [a] -> (a, [a])
-- Precond: La lista no debe ser vacia
splitHead [] = error "la lista no puede ser vacia"
splitHead xs = (elPrimero xs, sinElPrimero xs)