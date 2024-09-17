module MapV3(Map, emptyM, assocM, lookupM, deleteM, keys)

where
import Data.List (elemIndex)
data Map k v = M [k] [v]
{-INV.REP.: En Map ks vs 
    * ks y vs tienen la misma longitud.
    * la posicon de un clave dentro de ks, es la posicion de su valor dentro de vs
-}

emptyM :: Map k v -- O(1)
--Propósito: devuelve un map vacío
emptyM = M [] []

assocM :: Eq k => k -> v -> Map k v -> Map k v -- O(1)
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M ks vs) = M (k:ks) (v:vs)

lookupM :: Eq k => k -> Map k v -> Maybe v -- O(n)
--Propósito: encuentra un valor dado una clave.
lookupM k (M ks vs) = buscarEn k ks vs

deleteM :: Eq k => k -> Map k v -> Map k v -- O(n)
--Propósito: borra una asociación dada una clave.
deleteM k (M ks vs) = if elemIndex k ks /= Nothing
                            then M (sinElemento (valorDeMaybe (elemIndex k ks)) ks) (sinElemento (valorDeMaybe (elemIndex k ks)) vs)
                            else M ks vs

keys :: Eq k => Map k v -> [k]
--Propósito: devuelve las claves del map -- O(n)
keys (M ks vs) = sinRepetidos ks

buscarEn :: Eq k => k -> [k] -> [v] -> Maybe v -- O(n)
buscarEn k [] _ = Nothing
buscarEn k (k':ks) (v:vs) = if k == k'
                                then Just v
                                else buscarEn k ks vs

sinRepetidos :: Eq k => [k] -> [k] -- O(n) Donde n es la lista sobre la que se hace RE.
sinRepetidos []     = []
sinRepetidos (k:ks) = if elem k ks 
                      then sinRepetidos ks 
                      else k : (sinRepetidos ks)

valorDeMaybe :: Maybe a -> a -- O(1)
valorDeMaybe (Just x) = x
valorDeMaybe Nothing = error "no existe un valor valido"

sinElemento :: Int -> [a] -> [a]
--PROP: Dada una posicion, elimina el elemento de esa posicion.
--PRECON: debe haber un elemento en la posicion dada.
sinElemento 0 [x] = [x]
sinElemento 0 (x:xs) = xs
sinElemento n (x:xs) = x : sinElemento (n-1) xs