module MultiSetV1(MultiSet,emptyMS, addMS, ocurrencesMS, multiSetToList)   

where

import MapV1

data MultiSet a = MS (Map a Int)

emptyMS :: MultiSet a
--Propósito: denota un multiconjunto vacío.
emptyMS = MS emptyM

addMS :: Ord a => a -> MultiSet a -> MultiSet a
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS x (MS m) = if lookupM x m /= Nothing
                    then MS (assocM x 1 m)
                    else MS (assocM x (fromJust (lookupM x m) + 1) m)

ocurrencesMS :: Ord a => a -> MultiSet a -> Int
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
ocurrencesMS x (MS m) = if lookupM x m /= Nothing
                        then fromJust (lookupM x m)
                        else 0
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
unionMS ms1 ms2 = agregarLista ms1 (multiSetToList ms2)

agregarLista :: Ord a => MultiSet a -> [(a, Int)] -> MultiSet a
-- prop: dado un multi set y una lista de pares de elementos con un numero, agrega ese elemento n veces al multi set
agregarLista m [] = m
agregarLista m ((x,n):xns) = agregarLista (agregarVeces x n m) xns

agregarVeces :: Ord a => a -> Int -> MultiSet a -> MultiSet a
agregarVeces _ 0 m = m
agregarVeces x n m = agregarVeces x (n-1) (addMS x m)

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
intersectionMS  ms1 ms2 = agregarLista emptyMS (elementosEnComun (multiSetToList ms1) (multiSetToList ms2))

elementosEnComun :: Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
--prop: dadas dos lista de pares de elementos y numeros, devuelve una lista de pares con los elementos en comun de las dos listas 
elementosEnComun xns [] =
elementosEnComun [] xns =
elementosEnComun [] []  = []
elementosEnComun xns (yn:yns) =

multiSetToList :: Eq a => MultiSet a -> [(a, Int)] --(tuve que agregar Eq)
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.
multiSetToList (MS m) = mapToList m

fromJust :: Maybe a -> a -- O(1)
fromJust Nothing  = error "No hay elemento"
fromJust (Just x) = x

mapToList :: Eq k => Map k v -> [(k, v)] --O(n*2)
--Propósito: convierte un map en una lista de pares clave valor.
mapToList m = pasarALista (keys m) m

pasarALista :: Eq k => [k] ->  Map k v -> [(k, v)] --O(n*2)
pasarALista [] m = []
pasarALista (k:ks) m = (k,fromJust (lookupM k m)) : pasarALista ks m
