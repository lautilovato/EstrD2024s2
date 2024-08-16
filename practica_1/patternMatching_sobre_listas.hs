estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (x:_) = x