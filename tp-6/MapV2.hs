module MapV2(Map, emptyM, assocM, lookupM, deleteM, keys)

where

data Map k v = M [(k,[v])]

emptyM :: Map k [v] -- O(1)
--Propósito: devuelve un map vacío
emptyM = M []

assocM :: Eq k => k -> v -> Map k v -> Map k v -- O(n)
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M kvs) = M (agregar k v kvs)

lookupM :: Eq k => k -> Map k v -> [Maybe v] -- O(n)
--Propósito: encuentra un valor dado una clave.
lookupM k (M kvs) = buscarEn k kvs

deleteM :: Eq k => k -> Map k v -> Map k v -- O(n)
--Propósito: borra una asociación dada una clave.
deleteM k (M kvs) = M (eliminarEn k kvs)

keys :: Map k [v] -> [k]
--Propósito: devuelve las claves del map -- O(n)
keys (M kvs) = clavesDe kvs

agregar :: Eq k => k -> v -> [(k,[v])] -> [(k,[v])] -- O(n)
agregar k v [] = [(k,[v])]
agregar k v (kv:kvs) = if fst kv == k
                            then (k,v:(snd kv)) : kvs
                            else kv : agregar k v kvs

buscarEn :: Eq k => k -> [(k,[v])] -> [Maybe v] -- O(n)
buscarEn k [] = [Nothing]
buscarEn k (kv:kvs) = if fst kv == k
                            then valoresDeClave (snd kv)
                            else buscarEn k kvs

eliminarEn :: Eq k => k -> [(k,[v])] -> [(k,[v])] -- O(n)
eliminarEn _ [] = []
eliminarEn k (kv:kvs) = if fst kv == k
                            then kvs
                            else kv : eliminarEn k kvs

clavesDe :: [(k,[v])]-> [k] -- O(n)
clavesDe [] = []
clavesDe (kv:kvs) = fst kv : clavesDe kvs

valoresDeClave :: [a] -> [Maybe a]
valoresDeClave [] = []
valoresDeClave (x:xs) = Just x : valoresDeClave xs