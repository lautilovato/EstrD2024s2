module QueueV2
(Queue,emptyQ, isEmptyQ, enqueue, firstQ, dequeue )

where 

data Queue a = Q [a]

emptyQ :: Queue a
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a
enqueue x (Q ys) = Q (x:ys)

firstQ :: Queue a -> a
firstQ (Q xs) = last xs -- last [a] : devuelve el ultimo elemento de la lista

dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (init xs) -- init [a]: devuelve la lista sin el ultimo elemento 

--2. Implemente ahora la versión que agrega por delante y quita por final de la lista. Compare
--la eficiencia entre ambas implementaciones


