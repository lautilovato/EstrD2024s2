import PriorityQueueV1

--import MapV1
import MapV2

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

-- 3.1 - 3.4
{-
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
pasarALista (k:ks) m = (k,valorDeMaybe (lookupM k m)) : pasarALista ks m

valorDeMaybe :: Maybe a -> a -- O(1)
valorDeMaybe (Just x) = x
valorDeMaybe Nothing = error "no existe un valor valido"
-}

agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq [] = emptyM
agruparEq (kv:kvs) = assocM (fst kv) ([snd kv]) (agruparEq kvs)


valuesEj = lookupM "lautaro" (agruparEq listaKV)