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

consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (y:ys) = (x : y) : consACada x ys

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
