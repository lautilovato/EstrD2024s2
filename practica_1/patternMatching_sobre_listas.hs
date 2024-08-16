estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs

-- precon: la lista no debe ser vacia 
splitHead :: [a] -> (a, [a])
splitHead xs = (elPrimero xs, sinElPrimero xs)