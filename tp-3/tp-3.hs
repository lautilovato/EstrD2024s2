-- Punto 1.1

data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

nroBolitas :: Color -> Celda -> Int
{-Dados un color y una celda, indica la cantidad de bolitas de ese color. 
Nota: pensar si ya existe una operación sobre listas que ayude a resolver el problema-}
nroBolitas _ CeldaVacia = 0
nroBolitas colX (Bolita colY cel) = unoSiCeroSino (esDeColor colY colX) + nroBolitas colX cel


unoSiCeroSino:: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

esDeColor :: Color -> Color -> Bool
esDeColor Azul Azul = True 
esDeColor Rojo Rojo = True
esDeColor _ _ = False

celda0 = Bolita Rojo (Bolita Azul (Bolita Rojo CeldaVacia))

poner :: Color -> Celda -> Celda
poner col cel = (Bolita col cel) 

sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar colX (Bolita colY cel) = if esDeColor colX colY
                                    then cel
                                    else Bolita colY (sacar colX cel)

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ c = c
ponerN n col cel = ponerN (n-1) col (Bolita col cel)

-- Punto 1.2

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

camino1 = Nada (Nada Fin)
camino2 = Nada (Nada (Cofre [Cacharro] Fin))
camino3 = Nada (Nada (Cofre [Cacharro,Tesoro] (Nada (Cofre [Tesoro,Tesoro] Fin ))))

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre objs c) = hayTesoroEnObjetos objs || hayTesoro c
hayTesoro (Nada c)= hayTesoro c

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos [] = False
hayTesoroEnObjetos (obj:objs) = esTesoro obj || hayTesoroEnObjetos objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

pasosHastaTesoro :: Camino -> Int
--precond: existe al menos un tesoro en el camino
pasosHastaTesoro Fin = error "No se encontro un tesoro"
pasosHastaTesoro (Cofre objs c) = if hayTesoroEnObjetos objs
                                        then 0
                                        else pasosHastaTesoro c
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c


hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False
hayTesoroEn 0 c = esCaminoConTesoro c
hayTesoroEn n c = hayTesoroEn (n-1) (siguienteCamino c)

esCaminoConTesoro :: Camino -> Bool
esCaminoConTesoro (Cofre objs _) = hayTesoroEnObjetos objs
esCaminoConTesoro _ = False

siguienteCamino :: Camino -> Camino        
-- precond = el camino dado no puede ser un camino "Fin"
siguienteCamino (Cofre _ c) = c
siguienteCamino (Nada c) = c

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n Fin = n <= 0
alMenosNTesoros n (Cofre objs c) = (n - tesorosEnObjetos objs) <= 0 || alMenosNTesoros (n - tesorosEnObjetos objs) c
alMenosNTesoros n (Nada c) = n <= 0 || alMenosNTesoros n c

tesorosEnCamino :: Camino -> Int
-- indica cuantos tesoros hay en el camino
tesorosEnCamino (Cofre objs _) = tesorosEnObjetos objs
tesorosEnCamino _ = 0

tesorosEnObjetos :: [Objeto] -> Int
tesorosEnObjetos [] = 0
tesorosEnObjetos (obj:objs) = unoSiCeroSino (esTesoro obj) + tesorosEnObjetos objs 

cantTesorosEntre :: Int -> Int -> Camino -> Int
-- Precond: El segundo numero es igual o mayor que el primero 
cantTesorosEntre _ _ Fin = 0 
cantTesorosEntre 0 0 c  = tesorosEnCamino c 
cantTesorosEntre 0 m c  = tesorosEnCamino c + cantTesorosEntre 0 (m-1) (siguienteCamino c)
cantTesorosEntre n m c  = cantTesorosEntre (n-1) (m-1) (siguienteCamino c) 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

tree1 :: Tree Int
tree1 = NodeT 10 (NodeT 20 EmptyT (NodeT 30 EmptyT EmptyT)) EmptyT

tree2 = NodeT 20 (NodeT 10 (NodeT 30 EmptyT EmptyT) (NodeT 40 EmptyT EmptyT)) (NodeT 50 EmptyT EmptyT)

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x t1 t2) = 1 + sizeT t1 + sizeT t2

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n t1 t2) = (NodeT (2*n) (mapDobleT t1) (mapDobleT t2))

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT = False
perteneceT x (NodeT y t1 t2) = x == y || perteneceT x t1 || perteneceT x t2

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e EmptyT = 0
aparicionesT e (NodeT x t1 t2) = unoSiCeroSino (e == x) + aparicionesT e t1 + aparicionesT e t2

leaves :: Tree a -> [a]
-- NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.
leaves EmptyT = []
leaves (NodeT x t1 t2) = singularSi x (esVacio t1 && esVacio t2) ++ leaves t1 ++ leaves t2

singularSi :: a -> Bool -> [a]
singularSi x True = x:[]
singularSi _ False = []

esVacio :: Tree a -> Bool 
esVacio EmptyT = True
esVacio _ = False

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = (NodeT x (mirrorT t2) (mirrorT t1))

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x t1 t2) = toList t1 ++ [x] ++ toList t2

elementosDelArbol :: Tree a -> [a]
elementosDelArbol EmptyT = []
elementosDelArbol (NodeT x t1 t2) = [x] ++ (elementosDelArbol t1) ++ (elementosDelArbol t2)
  
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN n (NodeT x t1 t2) = if n == 0
                                then [x]
                                else levelN (n-1) t1 ++ levelN (n-1) t2

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2 ) = [x] : juntarNiveles (listPerLevel t1) (listPerLevel t2) 

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] yss = yss
juntarNiveles xss [] = xss 
juntarNiveles (xs:xss) (ys:yss) =  (xs ++ ys) : juntarNiveles xss yss 


ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = if heightT t1 > heightT t2
                                        then x : ramaMasLarga t1
                                        else x : ramaMasLarga t2

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT        = [] 
todosLosCaminos (NodeT x t1 t2) = [x] : consACada x (todosLosCaminos t1)
                             ++ consACada x (todosLosCaminos t2)

consACada :: a -> [[a]] -> [[a]]
consACada e []       = []
consACada e (x:xs)   = (e:x) : consACada e xs 

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
    deriving Show

suma = Sum ((Neg (Valor 2))) (Prod (Valor 2) (Valor 3))
dobleNegado = (Neg (Neg (Valor 1)))
prodCon0 = (Prod (Valor 0) (Valor 10))
prodCon1 = (Prod (Valor 1) (Valor 10))
sumCon0 = (Sum (Prod (Valor 0) (Valor 5)) (Valor 10))

eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum n m)  = (eval n) + (eval m)
eval (Prod n m) = (eval n) * (eval m)
eval (Neg n) = - (eval n)

simplificar :: ExpA -> ExpA
simplificar (Sum (Valor 0) x)  = simplificar x 
simplificar (Sum x (Valor 0))  = simplificar x
simplificar (Prod (Valor 0) x) = Valor 0 
simplificar (Prod x (Valor 0)) = Valor 0
simplificar (Prod (Valor 1) x) = simplificar x
simplificar (Prod x (Valor 1)) = simplificar x
simplificar (Neg (Neg x) )     = simplificar x
simplificar (Sum e1 e2)        = Sum  (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2)       = Prod (simplificar e1) (simplificar e2)
simplificar (Neg e)            = Neg  (simplificar e)
simplificar x                  = x

