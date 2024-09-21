module SetV1
(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

where 

data Set a = S [a]
{- INV.REP.: en S [a]
      * [a] no tiene elementos repetidos
-}

emptyS :: Set a
emptyS = S []

addS ::  Eq a => a -> Set a -> Set a
addS x (S ys) = if elem x ys 
                then S ys
                else S (x:ys)

sizeS :: Eq a => Set a -> Int
sizeS (S xs) = length xs

belongs :: Eq a => a -> Set a -> Bool
belongs x (S ys) = elem x ys 
    

removeS :: Eq a => a -> Set a -> Set a -- O(n)
removeS x (S ys) = if elem x ys
                        then S (eliminarElemento x ys)
                        else S ys

eliminarElemento :: Eq a => a -> [a] -> [a]
-- precond: elemento esta en la lista
eliminarElemento _ [] = error "el elemento no esta en la lista"
eliminarElemento x (y:ys) = if x == y 
                                then ys
                                else y : eliminarElemento x ys

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S (agregarSinRepetir xs ys)

agregarSinRepetir :: Eq a => [a] -> [a] -> [a]
agregarSinRepetir xs [] = xs
agregarSinRepetir [] ys = ys
agregarSinRepetir xs (y:ys) = if (elem y xs)
                                  then agregarSinRepetir xs ys
                                  else agregarSinRepetir (y:xs) ys

setToList :: Eq a => Set a -> [a]
setToList (S xs) = xs 
