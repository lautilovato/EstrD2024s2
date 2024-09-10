module SetV2
(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

where 

data Set a = S [a]

emptyS :: Set a
emptyS = S []

addS ::  Eq a => a -> Set a -> Set a
addS x (S ys) = (S (x:ys))

belongs :: Eq a => a -> Set a -> Bool
belongs x (S ys) = elem x ys 

sizeS :: Eq a => Set a -> Int
sizeS (S []) = 0
sizeS (S (x:xs)) = let sizeActual = sizeS (S xs)
                        in if (elem x xs)
                                then sizeActual
                                else 1 + sizeActual


removeS :: Eq a => a -> Set a -> Set a
removeS x (S ys) = S (eliminarElemento x ys)

eliminarElemento :: Eq a => a -> [a] -> [a]
-- elimina un elemento de la lista dada, el elemento puede estar mas de una vez
eliminarElemento _ [] = []
eliminarElemento x (y:ys) = if x == y 
                                then eliminarElemento x ys
                                else y : eliminarElemento x ys

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (xs++ys)

setToList :: Eq a => Set a -> [a]
setToList (S xs) = eliminarRepetidos xs 

eliminarRepetidos :: Eq a => [a] -> [a] 
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = let elementos = eliminarRepetidos xs
                                in if elem x xs
                                        then elementos
                                        else x : elementos
