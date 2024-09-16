module PriorityQueueV1
(PriorityQueue,emptyPQ,isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)

where

data PriorityQueue a = PQ [a]
{- INV.REP.: En PQ xs, lo elementos de xs estan ordenados de menor a mayor
-}


emptyPQ :: PriorityQueue a -- O(1)
--Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool -- O(1)
--Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a -- O(n)
--Propósito: inserta un elemento en la priority queue.
insertPQ x (PQ xs) = PQ (insertarEn x xs)

findMinPQ :: Ord a => PriorityQueue a -> a -- O(1)
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMinPQ (PQ xs) = head xs

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a -- O(1)
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ (PQ xs) = (PQ (tail xs)) 

insertarEn :: Ord a => a -> [a] -> [a] -- O(n)
insertarEn x [] = [x]
insertarEn x (y:ys) = if x <= y
                        then x : y : ys
                        else y : insertarEn x ys

