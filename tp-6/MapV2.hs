module MapV2(Map, emptyM, assocM, lookupM, deleteM, keys)

where

data Map k v = M [(k,v)]

emptyM :: Map k v -- O(1)
--Propósito: devuelve un map vacío
emptyM = M []

assocM :: Eq k => k -> v -> Map k v -> Map k v -- O(n)
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M kvs) = M ((k,v):kvs)

lookupM :: Eq k => k -> Map k v -> Maybe v -- O(n)
--Propósito: encuentra un valor dado una clave.
lookupM k (M kvs) = buscarEn k kvs

deleteM :: Eq k => k -> Map k v -> Map k v -- O(n)
--Propósito: borra una asociación dada una clave.
deleteM k (M kvs) = M (eliminarEn k kvs)

keys :: Eq k => Map k v -> [k]
--Propósito: devuelve las claves del map -- O(n)
keys (M kvs) = sinRepetidos (clavesDe kvs)

buscarEn :: Eq k => k -> [(k,v)] -> Maybe v -- O(n)
buscarEn k [] = Nothing
buscarEn k (kv:kvs) = if fst kv == k
                            then Just (snd kv)
                            else buscarEn k kvs

eliminarEn :: Eq k => k -> [(k,v)] -> [(k,v)] -- O(n)
eliminarEn _ [] = []
eliminarEn k (kv:kvs) = if fst kv == k
                            then kvs
                            else kv : eliminarEn k kvs

clavesDe :: [(k,v)]-> [k] -- O(n)
clavesDe [] = []
clavesDe (kv:kvs) = fst kv : clavesDe kvs

sinRepetidos :: Eq k => [k] -> [k] -- O(n) Donde n es la lista sobre la que se hace RE.
sinRepetidos []     = []
sinRepetidos (k:ks) = if elem k ks 
                      then sinRepetidos ks 
                      else k : (sinRepetidos ks)