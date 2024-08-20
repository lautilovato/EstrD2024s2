sumarTodos :: [Int] -> Int 
sumarTodos [] = 0
sumarTodos (n:ns) = n + sumarTodos ns 

longitud :: [a] -> Int
longitud [] = 0
longitud (n:ns) = 1 + longitud ns

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x:xs) ys =  x : agregar xs ys

hayAlMenosUnCinco :: [Int] -> Bool
hayAlMenosUnCinco [] = False
hayAlMenosUnCinco (n:ns) = n == 5 || hayAlMenosUnCinco ns

hayAlMenosUn :: Int -> [Int] -> Bool
hayAlMenosUn k [] = False
hayAlMenosUn k (n:ns) = n == k || hayAlMenosUn k ns

soloLosMayores :: Int -> [Int] -> [Int]
soloLosMayores k [] = []
soloLosMayores k (n:ns) = if n > k
                            then n : soloLosMayores k ns
                            else soloLosMayores k  ns

iniciales :: [String] -> [Char]
iniciales [] = []
iniciales (x:xs) = inicial x : iniciales xs

inicial :: String -> Char
inicial (s:ss) = s 

-- Recursión ANIDADA
miZip :: [a] -> [b] -> [(a,b)]
miZip []  _ = []
miZip _ [] = []
miZip (x:xs) (y:ys) = (x,y) : zip xs ys 


{-
ultimo :: [a] -> a -- precon: La lista NO puede ser vacía
ultimo [] = error "No hay elementos"
ultimo (x:xs) =   if null xs
                    then x
                    else ultimo xs
-}

-- es la misma funcion con mejor sintaxis 
ultimo :: [a] -> a -- precon: La lista NO puede ser vacía
ultimo [] = error "No hay elementos"
ultimo (x:[]) = x
ultimo (_:xs) = last xs

promedio :: [Int] -> Int -- precon: la lista no puede ser vacia
promedio ns = div (sumarTodos ns) (longitud ns)


-- Recursion sobre numeros 

sumatoria :: Int -> Int 
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1) 

replicar :: Int -> a -> [a]
-- Precon: el numero es mayor o igual que cero 
replicar 0 _ = []
replicar n x = x : replicar (n-1) x 

cuentaRegresivaDesde :: Int -> [Int]
cuentaRegresivaDesde 0 = [0]
cuentaRegresivaDesde n = n : cuentaRegresivaDesde (n-1)

losPrimerosN :: Int -> [Int] -> [Int]
losPrimerosN 0 _      = []
losPrimerosN _ []     = []
losPrimerosN k (n:ns) = n : losPrimerosN (k-1) ns