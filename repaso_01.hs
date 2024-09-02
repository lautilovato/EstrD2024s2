tomarHasta :: Int -> [a] -> [a]
--precond el numero debe ser >= 0
tomarHasta 0 _  = []
tomarHasta _ [] = []
tomarHasta n (x:xs) =  x : tomarHasta (n-1) xs

lista = [1,2,3,4,5]
lista2 = [4564,64,632,52,4,6,37,2,23,45,2]

tomarDesde :: Int -> [a] -> [a]
tomarDesde 0 xs = xs
tomarDesde _ [] = []
tomarDesde n (x:xs) = tomarDesde (n-1) xs

tomarEntre :: Int -> Int -> [a] -> [a]
-- precond: el segundo numero debe ser mayor o igual al primero 
tomarEntre n m xs =  tomarHasta (m-n+1) (tomarDesde n xs)

apariciones :: Eq a => [a] -> [(a, Int)]
apariciones [] = []
apariciones (x:xs) = agregar x (apariciones xs)

agregar :: Eq a =>  a -> [(a, Int)] -> [(a, Int)]
agregar x [] = [(x,1)]
agregar x ((y,n):yns) = if x == y
                        then (y,(n+1)) : yns
                        else (y,n) : agregar x yns


indexar :: [a] -> [(Int,a)]
indexar [] = []
indexar (x:xs) = (0,x) : aumentar (indexar xs)

aumentar :: [(Int,a)] -> [(Int,a)]
aumentar [] = []
aumentar ((i,y):xs) = (i+1,y) : aumentar xs

indexarDesde :: Int -> [a] -> [(Int,a)]
indexarDesde n [] = []
indexarDesde n (x:xs) = (n,x) : indexarDesde (n+1) xs


ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = insert x (ordenar xs)

insert :: Ord a => a -> [a] -> [a]
--precond: la lista esta ordenada
insert x [] = [x]
insert x (y:ys) = if x <= y
                        then x : y : ys
                        else y : insert x ys

prefijos :: [a] -> [[a]]
prefijos []     = []
prefijos (x:xs) = [x] : consACada x (prefijos xs)

consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (y:ys) = (x : y) : consACada x ys