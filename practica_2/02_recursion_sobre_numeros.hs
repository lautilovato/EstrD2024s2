factorial :: Int -> Int 
-- precond: el numero debe ser mayor o igual a cero
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
--precond: el numero debe ser mayor o igual a 1
repetir 0 _ = []
repetir n x = x : repetir (n-1) x
 

losPrimeros :: Int -> [a] -> [a]
-- precond: el numero debe ser mayor o igual a 1
losPrimeros 0 _  = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs 

sinLosPrimeros :: Int -> [a] -> [a]
-- precon: el numero debe ser mayor o igual a cero, y la lista debe tener al menos n elementos 
sinLosPrimeros 0 x = x
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs