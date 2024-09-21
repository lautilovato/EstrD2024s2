import PriorityQueueV1

import MapV1

ejPQ = insertPQ 3
     $ insertPQ 5
     $ insertPQ 1
     $ emptyPQ

{-Ejercicio 2
Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
menor a mayor utilizando una Priority Queue como estructura auxiliar. Cuál es su costo?
OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority
Queues basada en una estructura concreta llamada Heap, que será trabajada en la siguiente
práctica-}

--INTERFAZ: (emptyPQ,isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)

-- el costo de heapSort depende del costo de "insertPQ" en este caso es cuadratico

heapSort :: Ord a => [a] -> [a] -- O(n) 
heapSort xs = pasarAlista (agregarLista xs emptyPQ) -- O(n*2)

agregarLista :: Ord a => [a] -> PriorityQueue a -> PriorityQueue a -- O(n*2) 
agregarLista [] pq = pq
agregarLista (x:xs) pq = agregarLista xs (insertPQ x pq)

pasarAlista :: Ord a => PriorityQueue a -> [a] -- O(n)
pasarAlista pq = if isEmptyPQ pq
                        then []
                        else findMinPQ pq : pasarAlista (deleteMinPQ pq)


-- =============================
--    Map (diccionario)
-- =============================

--INTERFAZ:(emptyM, assocM, lookupM, deleteM, keys)

--Implementar como usuario del tipo abstracto Map las siguientes funciones:

ejM  = assocM "lautaro" "21"
     $ assocM "martin" "22"
     $ assocM "tomas" "20"
     $ emptyM

listaKV = [("lautaro", "21"),("martin", "22"),("tomas", "20"),("lautaro", "23")]

valuesM :: Eq k => Map k v -> [Maybe v] -- O(n*2)
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = valoresDe (keys m) m

valoresDe :: Eq k => [k] -> Map k v -> [Maybe v] -- O(n*2)
-- PROP: dada una lista de claves y un map, devuelve los valores de esas claves
valoresDe [] m = []
valoresDe (k:ks) m = lookupM k m : valoresDe ks m

todasAsociadas :: Eq k => [k] -> Map k v -> Bool -- O(n*2)
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas [] m = True
todasAsociadas (x:xs) m = elem x (keys m) && todasAsociadas xs m

listToMap :: Eq k => [(k, v)] -> Map k v --O(n*2)
--Propósito: convierte una lista de pares clave valor en un map.
listToMap [] = emptyM
listToMap (kv:kvs) = assocM (fst kv) (snd kv) (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k, v)] --O(n*2)
--Propósito: convierte un map en una lista de pares clave valor.
mapToList m = pasarALista (keys m) m

pasarALista :: Eq k => [k] ->  Map k v -> [(k, v)] --O(n*2)
pasarALista [] m = []
pasarALista (k:ks) m = (k,fromJust (lookupM k m)) : pasarALista ks m

fromJust :: Maybe a -> a -- O(1)
fromJust (Just x) = x
fromJust Nothing = error "no existe un valor valido"


agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq []       = emptyM
agruparEq (kv:kvs) = agruparClaves kv (agruparEq kvs)

agruparClaves :: Eq k => (k,v) -> Map k [v] -> Map k [v]
agruparClaves (k,v) m = case lookupM k m of 
                              Nothing  -> assocM k [v] m
                              Just vs  -> assocM k (v:vs) m

mapInt :: Map [Char] Int
mapInt = assocM "a" 10
       $ assocM "b" 7 
       $ assocM "c" 8
       $ assocM "d" 9
       $ assocM "e" 5 emptyM


incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar [] m = m
incrementar (k:ks) m = let valorK = lookupM k m 
                       in if valorK == Nothing
                              then incrementar ks m
                              else incrementar ks (assocM k ((fromJust valorK) + 1) m)


mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps m1 m2 = agregarListaAMap (mapToList m1) m2

agregarListaAMap :: Eq k => [(k,v)] -> Map k v -> Map k v
-- PROP: dada una lista de pares clave valor, las agrega a un map
agregarListaAMap [] m = m
agregarListaAMap (kv:kvs) m = agregarListaAMap kvs (assocM (fst kv) (snd kv) m)

indexar :: [a] -> Map Int a
--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista
indexar xs = indexarEnMap xs emptyM

indexarEnMap :: [a] -> Map Int a -> Map Int a
indexarEnMap [] m = m
indexarEnMap (x:xs) m = indexarEnMap xs ( assocM (length (keys m)) x m)

ocurrencias :: String -> Map Char Int
--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen 
--en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias str = agregarStringAMap str emptyM

agregarStringAMap :: String -> Map Char Int -> Map Char Int
agregarStringAMap [] m = m
agregarStringAMap (ch:chs) m = if lookupM ch m == Nothing
                                        then agregarStringAMap chs (assocM ch 1 m)
                                        else agregarStringAMap chs (assocM ch (fromJust (lookupM ch m) + 1) m)

                                        