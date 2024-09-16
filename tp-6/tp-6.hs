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

valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = valoresDe (keys m) m

valoresDe :: Eq k => [k] -> Map k v -> [Maybe v]
-- PROP: dada una lista de claves y un map, devuelve los valores de esas claves
valoresDe [] m = []
valoresDe (k:ks) m = lookupM k m : valoresDe ks m

ejM  = assocM "lautaro" "21"
     $ assocM "martin" "22"
     $ assocM "tomas" "20"
     $ emptyM
     
