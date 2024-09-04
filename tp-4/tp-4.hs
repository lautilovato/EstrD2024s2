data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
    deriving Show

pizza = (Capa Salsa (Capa Queso (Capa Jamon (Capa Salsa Prepizza))))
ingredientes = [Jamon, Queso, Salsa]
pizza2 = (Capa (Aceitunas 4)(Capa Queso Prepizza))

cantidadDeCapas :: Pizza -> Int
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p 

armarPizza :: [Ingrediente] -> Pizza
--Dada una lista de ingredientes construye una pizza
armarPizza [] = Prepizza
armarPizza (ig:igs) =  (Capa ig (armarPizza igs))

sacarJamon :: Pizza -> Pizza
-- Cambiar 
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ig p) = if esJamon ig
                            then sacarJamon p
                            else (Capa ig (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False

tieneSoloSalsaYQueso :: Pizza -> Bool
--Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
--particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ig p) = esSalsaOQueso ig && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _     = False

duplicarAceitunas :: Pizza -> Pizza
--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ig p) = if esAceituna ig 
                                    then (Capa (dobleDeAceitunas ig ) (duplicarAceitunas p)) 
                                    else (Capa ig (duplicarAceitunas p))

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas n) = True
esAceituna _ = False

dobleDeAceitunas ::Ingrediente -> Ingrediente
dobleDeAceitunas (Aceitunas n) = (Aceitunas (n*2))
dobleDeAceitunas _ = error "El ingrediente no es aceituna"

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = ((cantidadDeCapas p),p) : cantCapasPorPizza ps

--Punto 2

data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
    deriving Show

cofreT = (Cofre [Chatarra,Chatarra,Tesoro,Chatarra,Tesoro])
cofreC = (Cofre [Chatarra,Chatarra,Chatarra])
mapa = (Bifurcacion cofreT (Bifurcacion cofreC (Fin cofreC) (Fin cofreT)) (Fin cofreC))

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || (hayTesoro m1) || (hayTesoro m2)

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre objs) = hayTesoroEnObjs objs

hayTesoroEnObjs :: [Objeto] -> Bool
hayTesoroEnObjs [] = False
hayTesoroEnObjs (obj:objs) = esTesoro obj || hayTesoroEnObjs objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Bifurcacion c _ _ ) = hayTesoroEnCofre c 
hayTesoroEn [] (Fin c) = hayTesoroEnCofre c
hayTesoroEn (d:ds) (Fin c) = False
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzq d
                                                then hayTesoroEn ds m1 
                                                else hayTesoroEn ds m2

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False


caminoAlTesoro :: Mapa -> [Dir]
--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2 ) = if hayTesoroEnCofre c
                                            then [] 
                                            else if hayTesoro m1
                                                then Izq : caminoAlTesoro m1 
                                                else Der : caminoAlTesoro m2 

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = if heightTMapa m1 > heightTMapa m2
                                                    then Izq : caminoDeLaRamaMasLarga m1 
                                                    else Der : caminoDeLaRamaMasLarga m2

heightTMapa :: Mapa -> Int
heightTMapa (Fin _) = 0
heightTMapa (Bifurcacion _ m1 m2) = 1 + max (heightTMapa m1) (heightTMapa m2)


tesorosPorNivel :: Mapa -> [[Objeto]]
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c) = [tesorosEnCofre c]
tesorosPorNivel (Bifurcacion c m1 m2) = tesorosEnCofre c : juntarNiveles (tesorosPorNivel m1) (tesorosPorNivel m2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] yss = yss
juntarNiveles xss [] = xss 
juntarNiveles (xs:xss) (ys:yss) =  (xs ++ ys) : juntarNiveles xss yss 

tesorosEnCofre :: Cofre -> [Objeto]
tesorosEnCofre (Cofre objs) = tesorosEnObjetos objs

tesorosEnObjetos :: [Objeto] -> [Objeto]
tesorosEnObjetos [] = []
tesorosEnObjetos (obj:objs) = if esTesoro obj
                                    then obj : tesorosEnObjetos objs
                                    else tesorosEnObjetos objs

todosLosCaminos :: Mapa -> [[Dir]]
--Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin _) = [[]]
todosLosCaminos (Bifurcacion _ m1 m2) = [] : consACada Izq (todosLosCaminos m1) 
                                                            ++ consACada Der (todosLosCaminos m2) 


consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (y:ys) = (x : y) : consACada x ys

--Punto 3

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
data Nave = N (Tree Sector)
    deriving Show

miNave = (N (NodeT sector1 EmptyT (NodeT sector2 EmptyT EmptyT)))
sector1 = (S "S1" [Almacen [Torpedo, Combustible], Motor 2] ["t2", "t1"])
sector2 = (S "S2" [Motor 3, Almacen [Comida, Oxigeno]] ["t1", "t2"])

sectores :: Nave -> [SectorId]
sectores (N t) = sectoresDeTree t 

sectoresDeTree :: Tree Sector -> [SectorId]
sectoresDeTree EmptyT = []
sectoresDeTree (NodeT sec t1 t2) = idDelSector sec : sectoresDeTree t1 ++ sectoresDeTree t2

idDelSector :: Sector -> SectorId
idDelSector (S id _ _ ) = id

poderDePropulsion :: Nave -> Int
--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion (N t) = poderDePropulsionEnArbol t

poderDePropulsionEnArbol :: Tree Sector -> Int 
poderDePropulsionEnArbol EmptyT = 0
poderDePropulsionEnArbol (NodeT sec t1 t2) = poderSector sec + poderDePropulsionEnArbol t1 + poderDePropulsionEnArbol t2

poderSector :: Sector -> Int
poderSector (S _ cs _) = poderComponentes cs

poderComponentes :: [Componente] -> Int
poderComponentes [] = 0
poderComponentes (c:cs) = if esMotor c
                                then poderMotor c + poderComponentes cs
                                else poderComponentes cs

esMotor :: Componente -> Bool
esMotor (Motor _) = True 
esMotor _  = False

poderMotor :: Componente -> Int
poderMotor (Motor n) = n
poderMotor _ = error "este componente no es un motor"

barriles :: Nave -> [Barril]
barriles (N t) = barrilesEnArbol t

barrilesEnArbol :: Tree Sector -> [Barril]
barrilesEnArbol EmptyT = []
barrilesEnArbol (NodeT sec t1 t2) = barrilesEnSector sec ++ barrilesEnArbol t1 ++ barrilesEnArbol t2

barrilesEnSector :: Sector -> [Barril]
barrilesEnSector (S _ cs _) = barrilesEnComponentes cs

barrilesEnComponentes :: [Componente] -> [Barril]
barrilesEnComponentes [] = []
barrilesEnComponentes (c:cs) = if esAlmacen c
                                    then barrilesDeAlmacen c ++ barrilesEnComponentes cs
                                    else barrilesEnComponentes cs

esAlmacen :: Componente -> Bool
esAlmacen (Almacen bs) = True
esAlmacen _ = False

barrilesDeAlmacen :: Componente -> [Barril]
barrilesDeAlmacen (Almacen bs) = bs
barrilesDeAlmacen _ = error "aca no hay barriles"

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector cs sId (N t) = (N (agregarComponentes cs sId t))
                                

tieneId :: Sector -> SectorId -> Bool
--Indica si un sector tiene el ID dado
tieneId (S id _ _ ) sId = id == sId

agregarComponentes :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
-- agregar componentes en el sector con ID dado
agregarComponentes cs id EmptyT = EmptyT
agregarComponentes cs id (NodeT sec t1 t2) = if tieneId sec id
                                                then (NodeT (agregarComponentesASector cs sec) t1 t2)
                                                else (NodeT sec (agregarComponentes cs id t1) (agregarComponentes cs id t2) )

agregarComponentesASector :: [Componente] -> Sector -> Sector
agregarComponentesASector cs1 (S id cs2 ts) = (S id (cs1 ++ cs2) ts)

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA t sId (N tree) = (N (asignarTripulanteEnArbol t sId tree))

asignarTripulanteEnArbol :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteEnArbol _ [] t = t
asignarTripulanteEnArbol trip (sId:sIds) t = asignarTripulanteEnArbol trip sIds (asignarTripulanteEnSector trip sId t) 

asignarTripulanteEnSector :: Tripulante -> SectorId -> Tree Sector -> Tree Sector
asignarTripulanteEnSector t id EmptyT = EmptyT
asignarTripulanteEnSector t id (NodeT sec t1 t2) = if tieneId sec id
                                                then (NodeT (agregarTripulanteASector t sec) t1 t2)
                                                else (NodeT sec (asignarTripulanteEnSector t id t1) (asignarTripulanteEnSector t id t2) )

agregarTripulanteASector :: Tripulante -> Sector -> Sector
agregarTripulanteASector t (S id cs ts) = (S id cs (t:ts))


sectoresAsignados :: Tripulante -> Nave -> [SectorId]
-- Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados trip (N tree) =  sectoresEnArbol trip tree

sectoresEnArbol :: Tripulante -> Tree Sector -> [SectorId]
sectoresEnArbol _ EmptyT = []
sectoresEnArbol trip (NodeT s t1 t2) = if hayTripulanteEn trip s 
                                            then (idDelSector s) : sectoresEnArbol trip t1 ++ sectoresEnArbol trip t2
                                            else sectoresEnArbol trip t1 ++ sectoresEnArbol trip t2

hayTripulanteEn :: Tripulante -> Sector -> Bool
hayTripulanteEn t (S id cs ts) = pertenece t ts

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs

tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = tripulantesEnArbol t

tripulantesEnArbol :: Tree Sector -> [Tripulante]
tripulantesEnArbol EmptyT = []
tripulantesEnArbol (NodeT s t1 t2) = juntarSinRepetir (tripulantesDeSector s) (
                                     juntarSinRepetir (tripulantesEnArbol t1) 
                                                     (tripulantesEnArbol t2))

tripulantesDeSector :: Sector -> [Tripulante]
tripulantesDeSector (S _ _ ts) = ts 

juntarSinRepetir :: Eq a => [a] -> [a] -> [a]
juntarSinRepetir [] ys     = ys
juntarSinRepetir (x:xs) ys = singularSi x (not (pertenece x ys)) ++ juntarSinRepetir xs ys 

singularSi :: a -> Bool -> [a]
singularSi x True = x:[]
singularSi _ False = []