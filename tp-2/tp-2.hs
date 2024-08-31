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
elMinimo [] = error "La llista no puede ser vacia"
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
cuentaRegresiva n = if (n < 1)
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
sinLosPrimeros 0 xs = xs 
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs 

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
elMasViejo [] = error "La lista no puede ser vacia"
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
entrenador1 = ConsEntrenador "lala" [venasaur, charizard, squirtle, venasaur]
entrenador2 = ConsEntrenador "lele" [squirtle, squirtle, squirtle]

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador n ps) = if (not (estaVacia ps) )
                                            then unoSiCeroSino ( esDeTipo (tipoDelPokemon (head ps) ) t) + cantPokemonDe t (ConsEntrenador n (tail ps))
                                            else 0

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

tipoDelPokemon :: Pokemon -> TipoDePokemon
tipoDelPokemon (ConsPokemon t _) = t

esDeTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDeTipo Agua Agua   = True
esDeTipo Fuego Fuego = True
esDeTipo Planta Planta = True
esDeTipo _ _ = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador nx ps1) (ConsEntrenador ny ps2) = if (estaVacia ps1 || estaVacia ps2)
                                                                                            then 0
                                                                                            else unoSiCeroSino ( esDeTipo t (tipoDelPokemon (head ps1))  && leGanaALosPokemon (head ps1) ps2)
                                                                                                + cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador nx (tail ps1)) (ConsEntrenador ny ps2)


-- dado un pokemon indica si le gana a todos los otros pokemons 
leGanaALosPokemon :: Pokemon -> [Pokemon] -> Bool
leGanaALosPokemon x [] = True
leGanaALosPokemon x (p:ps) = esTipoSuperior (tipoDelPokemon x) (tipoDelPokemon p) && leGanaALosPokemon x ps

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
senior = Management Senior proyecto1
junior2 = Developer Junior proyecto2
semiSenior2 = Developer SemiSenior proyecto2

empresa = ConsEmpresa [junior, semiSenior, senior, junior2, semiSenior2]

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = eliminarProyectosDuplicados (proyectosRoles rs) 

proyectosRoles :: [Rol] -> [Proyecto]
proyectosRoles [] = []
proyectosRoles (r:rs) = proyectoDelRol r : proyectosRoles rs

proyectoDelRol :: Rol -> Proyecto
proyectoDelRol (Developer _ p) = p
proyectoDelRol (Management _ p) = p

eliminarProyectosDuplicados ::[Proyecto] -> [Proyecto]
eliminarProyectosDuplicados [] = []
eliminarProyectosDuplicados (x:xs) = if elProyectoEstaEn x xs
                                        then eliminarProyectosDuplicados xs
                                        else x : eliminarProyectosDuplicados xs

elProyectoEstaEn :: Proyecto -> [Proyecto] -> Bool
elProyectoEstaEn _ [] = False
elProyectoEstaEn x (p:ps) = nombreDelProyecto x == nombreDelProyecto p || elProyectoEstaEn x ps

nombreDelProyecto :: Proyecto -> String
nombreDelProyecto (ConsProyecto n) = n

rolesDeEmpresa :: Empresa -> [Rol]
rolesDeEmpresa (ConsEmpresa r) = r 

losDevSenior :: Empresa -> [Proyecto] -> Int
{-Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
además a los proyectos dados por parámetro.-}
losDevSenior _ [] = 0
losDevSenior e  (p:ps) = desarrolladoresSeniorQueTrabajanEn (rolesDeEmpresa e) p 
                                           +  losDevSenior e ps

desarrolladoresSeniorQueTrabajanEn :: [Rol] -> Proyecto -> Int
-- indica cuantos desarrolladores Senior Trabajan en un proyecto
desarrolladoresSeniorQueTrabajanEn [] _ = 0
desarrolladoresSeniorQueTrabajanEn (x:xs) p = unoSiCeroSino (esSenior (seniorityDelRol x) && trabajaEnProyecto x p)
                                                    +  desarrolladoresSeniorQueTrabajanEn xs p
esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _  = False

seniorityDelRol :: Rol -> Seniority
seniorityDelRol (Management x _ ) = x
seniorityDelRol (Developer x _ )  = x

trabajaEnProyecto :: Rol -> Proyecto -> Bool
trabajaEnProyecto r p = nombreDelProyecto (proyectoDelRol r) == nombreDelProyecto p

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
asignadosPorProyecto (ConsEmpresa rs) =  asignados rs 

asignados :: [Rol] ->  [(Proyecto, Int)] 
asignados []     = [] 
asignados (r:rs) = asignadosPorTuplas (proyectoDelRol r) (asignados rs) 

asignadosPorTuplas :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
asignadosPorTuplas p []            = (p,1) : [] 
asignadosPorTuplas p ((p1, n): ps) = if sonElMismoProyecto p p1 
                                          then ((p1, n+1): ps)  
                                          else ((p1, n)  : asignadosPorTuplas p ps)  

sonElMismoProyecto :: Proyecto -> Proyecto -> Bool 
sonElMismoProyecto p1 p2 = nombreDelProyecto p1 == nombreDelProyecto p2

