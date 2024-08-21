data Persona = P String Int 
              -- Nombre Edad 
    deriving Show

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (x:xs) = if esMayorA n x 
                        then x : mayoresA n xs
                        else mayoresA n xs

esMayorA :: Int -> Persona -> Bool 
esMayorA n (P s i) = n < i

lautaro = P "Lautaro" 21
david = P "David" 30

promedioEdad :: [Persona] -> Int
--Precon: la lista posee al menos una persona 
promedioEdad [] = 0
promedioEdad xs = div (sumaDeEdades xs)  (length xs)

sumaDeEdades :: [Persona] -> Int
sumaDeEdades [] = 0
sumaDeEdades (x:xs) = edad x + sumaDeEdades xs

edad :: Persona -> Int
edad (P n e) = e

elMasViejo :: [Persona] -> Persona
--Precon: la lista debe poseer al menos una persona
elMasViejo (x:[]) = x
elMasViejo (x:xs) = if esMasViejo x (elMasViejo xs)
                        then x
                        else elMasViejo xs

esMasViejo :: Persona -> Persona -> Bool
-- indica si la primero persona tiene mas edad que la segunda 
esMasViejo (P nx ex) (P ny ey) = ex > ey


--PUNTO 2
data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int 
data Entrenador = ConsEntrenador String [Pokemon] -- nombre pokemons

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador n ps) = length ps

charizard = ConsPokemon Fuego 10
venasaur = ConsPokemon Planta 5
squirtle = ConsPokemon Agua 7
entrenador = ConsEntrenador "lala" [venasaur, charizard, squirtle]

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ []) = 0
cantPokemonDe t (ConsEntrenador n ps) = unoSiCeroSino (esDeTipo (head ps) t) +
                                            cantPokemonDe t (ConsEntrenador n (tail ps))


unoSiCeroSino:: Bool -> Int
unoSiCeroSino b = if(b)
                    then 1
                    else 0

esDeTipo :: Pokemon -> TipoDePokemon -> Bool
esDeTipo p Agua   = esDeAgua p
esDeTipo p Fuego = esDeFuego p
esDeTipo p Planta = esDePlanta p 
esDeTipo _ _ = False

-- indica si el pokemon es de tipo agua 
esDeAgua :: Pokemon -> Bool
esDeAgua (ConsPokemon Agua _) = True 
esDeAgua (ConsPokemon _ _) = False

-- indica si el pokemon es de tipo fuego 
esDeFuego :: Pokemon -> Bool
esDeFuego (ConsPokemon Fuego _) = True 
esDeFuego (ConsPokemon _ _) = False

-- indica si el pokemon es de tipo planta  
esDePlanta :: Pokemon -> Bool
esDePlanta (ConsPokemon Planta _) = True 
esDePlanta (ConsPokemon _ _) = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían a los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ []) (ConsEntrenador _ _) = 0
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ _) (ConsEntrenador _ []) = 0
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador nx psx) (ConsEntrenador ny psy) = pokemonsDeTipoQueLeGanan t psx (head psy) + 
                                                                                            cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador nx psx) (ConsEntrenador ny (tail psy))

-- dada una lista de pokemons y un TipoDePokemon, indica cuantos del tipo dado le ganan a otro pokemon dado.
pokemonsDeTipoQueLeGanan :: TipoDePokemon -> [Pokemon] -> Pokemon -> Int
pokemonsDeTipoQueLeGanan t [] _ = 0
pokemonsDeTipoQueLeGanan t (x:xs) p = unoSiCeroSino (esDeTipo x t && esTipoSuperior (tipo x) (tipo p) ) + pokemonsDeTipoQueLeGanan t xs p

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua  = True
esTipoSuperior _ _ = False

tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t p) = t


esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador n ps) = hayPokemonDeTipo ps Fuego && hayPokemonDeTipo ps Planta && hayPokemonDeTipo ps Agua

hayPokemonDeTipo :: [Pokemon] -> TipoDePokemon -> Bool
-- dada una lista de pokemons, indica si hay al menos un pokemon del tipo dado
hayPokemonDeTipo [] _ = False
hayPokemonDeTipo (x:xs) t = esDeTipo x t || hayPokemonDeTipo xs t  