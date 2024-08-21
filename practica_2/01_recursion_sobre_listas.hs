sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns 

longitud :: [a] -> Int
longitud [] = 0
longitud (n:ns) = 1 + longitud ns

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (n:ns) = n + 1 : sucesores ns

conjuncion :: [Bool] -> Bool
conjuncion [] = False
conjuncion (False:_) =  False
conjuncion (True:[]) = True
conjuncion (True:xs) = conjuncion xs

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (True:_) = True 
disyuncion (False:xs) = disyuncion xs


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
