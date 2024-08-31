-- Punto 1

sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns 

longitud :: [a] -> Int
longitud [] = 0
longitud (n:ns) = 1 + longitud ns

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = n + 1 : sucesores ns
{-
conjuncion :: [Bool] -> Bool
conjuncion [] = False
conjuncion (False:_) =  False
conjuncion (True:[]) = True
conjuncion (True:xs) = conjuncion xs
-}
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs


aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) =  agregar x  (aplanar xs)

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) =  unoSiCeroSino (e == x) + apariciones e xs

unoSiCeroSino:: Bool -> Int
unoSiCeroSino b = if(b)
                    then 1
                    else 0

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = [] 
losMenoresA k (n:ns) = if n < k
                          then n : losMenoresA k ns
                          else losMenoresA k ns

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (x:xs) = if length x > n
                                    then x : lasDeLongitudMayorA n xs
                                    else lasDeLongitudMayorA n xs 


agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal xs y = xs ++ [y]

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x:xs) ys =  x : agregar xs ys

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos _ [] = []
zipMaximos [] _ = []
zipMaximos (x:xs) (y:ys) = if x > y 
                                then x : zipMaximos xs ys 
                                else y : zipMaximos xs ys

elMinimo :: Ord a => [a] -> a
--Precon: La lista no debe ser vacia 
elMinimo (x:[]) = x
elMinimo (x:xs) = if x < elMinimo xs
                        then x
                        else elMinimo xs

-- Punto 2 
factorial :: Int -> Int 
-- precond: el numero debe ser mayor o igual a cero
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = if (n < 0)
                        then []
                        else n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
--precond: el numero debe ser mayor o igual a 1
repetir 0 _ = []
repetir n x = x : repetir (n-1) x
 

losPrimeros :: Int -> [a] -> [a]
-- precond: el numero debe ser mayor o igual a 0
losPrimeros 0 _  = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs 

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 x = x
sinLosPrimeros n (x:xs) =   if length xs + 1 <= n
                                then [] 
                                else sinLosPrimeros (n-1) xs

-- Punto 3

data Persona = P String Int 
              -- Nombre Edad 
    deriving Show

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (x:xs) = if esMayorA n x 
                        then x : mayoresA n xs
                        else mayoresA n xs

esMayorA :: Int -> Persona -> Bool 
esMayorA n p = n < edad p

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
esMasViejo p1 p2 = edad p1 > edad p2


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
cantPokemonDe t (ConsEntrenador n ps) = unoSiCeroSino (esDeTipo (tipoDelPokemon (head ps) ) t) +
                                            cantPokemonDe t (ConsEntrenador n (tail ps))

tipoDelPokemon :: Pokemon -> TipoDePokemon
tipoDelPokemon (ConsPokemon t _) = t

esDeTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDeTipo Agua Agua   = True
esDeTipo Fuego Fuego = True
esDeTipo Planta Planta = True
esDeTipo _ _ = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían a los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ []) (ConsEntrenador _ _) = 0
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ _) (ConsEntrenador _ []) = 0
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador nx psx) (ConsEntrenador ny psy) = pokemonsDeTipoQueLeGanan t psx (head psy) + 
                                                                                            cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador nx psx) (ConsEntrenador ny (tail psy))

-- dada una lista de pokemons y un TipoDePokemon, indica cuantos del tipo dado le ganan a otro pokemon dado.
pokemonsDeTipoQueLeGanan :: TipoDePokemon -> [Pokemon] -> Pokemon -> Int
pokemonsDeTipoQueLeGanan t [] _ = 0
pokemonsDeTipoQueLeGanan t (x:xs) p = unoSiCeroSino (esDeTipo (tipoDelPokemon x) t && esTipoSuperior (tipoDelPokemon x) (tipoDelPokemon p) ) + pokemonsDeTipoQueLeGanan t xs p

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua  = True
esTipoSuperior _ _ = False

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador n ps) = hayPokemonDeTipo ps Fuego && hayPokemonDeTipo ps Planta && hayPokemonDeTipo ps Agua

hayPokemonDeTipo :: [Pokemon] -> TipoDePokemon -> Bool
-- dada una lista de pokemons, indica si hay al menos un pokemon del tipo dado
hayPokemonDeTipo [] _ = False
hayPokemonDeTipo (x:xs) t = esDeTipo (tipoDelPokemon x) t || hayPokemonDeTipo xs t  


data Seniority = Junior | SemiSenior | Senior
    deriving Show
data Proyecto = ConsProyecto String
    deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show
data Empresa = ConsEmpresa [Rol]
    deriving Show

proyecto1 = ConsProyecto "p1"
proyecto2 = ConsProyecto "p2"

junior = Developer Junior proyecto1
semiSenior = Developer SemiSenior proyecto1
senior = Management Senior proyecto2

empresa = ConsEmpresa [junior, semiSenior, senior]

proyectos :: Empresa -> [Proyecto]
--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos (ConsEmpresa []) = []
proyectos (ConsEmpresa (x:xs)) =    if (elProyectoEstaEn (proyecto x) (proyectosDeRoles xs))
                                        then proyectos (ConsEmpresa xs)
                                        else proyecto x : proyectos (ConsEmpresa xs)

elProyectoEstaEn :: Proyecto -> [Proyecto] -> Bool
elProyectoEstaEn p ps = pertenece (nombre p) (nombresDeProyectos ps)

proyecto :: Rol -> Proyecto
proyecto (Developer _ p) = p
proyecto (Management _ p) = p

nombre :: Proyecto -> String
nombre (ConsProyecto n) = n

nombresDeProyectos :: [Proyecto] -> [String]
nombresDeProyectos [] = []
nombresDeProyectos (x:xs) = nombre x : nombresDeProyectos xs

rolesDeEmpresa :: Empresa -> [Rol]
rolesDeEmpresa (ConsEmpresa r) = r 

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles [] = []
proyectosDeRoles (x:xs) = proyecto x : proyectosDeRoles xs


losDevSenior :: Empresa -> [Proyecto] -> Int
{-Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
además a los proyectos dados por parámetro.-}
losDevSenior _ [] = 0
losDevSenior e  (p:ps) = desarrolladoresSeniorQueTrabajanEn (rolesDeEmpresa e) p 
                                           +  losDevSenior e ps


desarrolladoresSeniorQueTrabajanEn :: [Rol] -> Proyecto -> Int
-- indica cuantos desarrolladores Senior Trabajan en un proyecto
desarrolladoresSeniorQueTrabajanEn [] _ = 0
desarrolladoresSeniorQueTrabajanEn (x:xs) p = unoSiCeroSino (esDesarroladorSenior x && trabajaEnProyecto x p)
                                                    +  desarrolladoresSeniorQueTrabajanEn xs p
esDesarroladorSenior :: Rol -> Bool
esDesarroladorSenior (Developer Senior _) = True
esDesarroladorSenior (Management Senior _ ) = True
esDesarroladorSenior _ = False

trabajaEnProyecto :: Rol -> Proyecto -> Bool
trabajaEnProyecto r p = nombre (proyecto r) == nombre p

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn (x:xs) e = empleadosQueTrabajanEn (rolesDeEmpresa e) x 
                                    + cantQueTrabajanEn xs e 


empleadosQueTrabajanEn :: [Rol] -> Proyecto -> Int
empleadosQueTrabajanEn [] _ = 0
empleadosQueTrabajanEn (x:xs) p = unoSiCeroSino (trabajaEnProyecto x p) 
                                        + empleadosQueTrabajanEn xs p


asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
{-Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas-}
asignadosPorProyecto (ConsEmpresa []) = []
asignadosPorProyecto (ConsEmpresa (d:ds))= (proyecto d, empleadosQueTrabajanEn ds (proyecto d) + 1) : 
                                                asignadosPorProyecto (empresaSinElProyecto (ConsEmpresa ds) (proyecto d))


empresaSinElProyecto :: Empresa -> Proyecto -> Empresa
-- dada una empresa y un proyecto, devuelve la empresa  sin los roles que trabajan en el proyecto 
empresaSinElProyecto e p = (ConsEmpresa (empleadosSinElProyecto (rolesDeEmpresa e) p))

empleadosSinElProyecto :: [Rol] -> Proyecto -> [Rol]
-- dada una lista de roles y un proyecto, devuelve los roles que no trabajan en el proyecto 
empleadosSinElProyecto [] _ = []
empleadosSinElProyecto (x:xs) p = if not (trabajaEnProyecto x p)
                                        then x : empleadosSinElProyecto xs p
                                        else empleadosSinElProyecto xs p

