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
laQueEsMayor (P nX eX ) (P nY eY ) = if (eX > eY )
                                        then (P nX eX )
                                        else (P nY eY )

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
superaA (Pok tX pX) (Pok tY pY) = if(esTipoSuperior tX tY)
                                    then True
                                    else False

-- indica si el primer tipo de pokemon es superior al segundo
esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua  = True
esTipoSuperior _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E n p1 p2) = if (esDeTipo p1 t && esDeTipo p2 t) then 2
                                    else if (esDeTipo p1 t || esDeTipo p2 t) then 1
                                    else 0

-- indicia si el pokemon es del tipo dado     
esDeTipo :: Pokemon -> TipoDePokemon -> Bool
esDeTipo (Pok Agua _) Agua   = True
esDeTipo (Pok Fuego _) Fuego = True
esDeTipo (Pok Planta _) Planta = True
esDeTipo _ _ = False


juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E nX p1 p2 ), (E nY p3 p4 )) = [p1, p2, p3, p4]