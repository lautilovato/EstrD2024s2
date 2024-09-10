module QueueV1
(Queue,emptyQ, isEmptyQ, enqueue, firstQ, dequeue )

where 

data Queue a = Q [a]

emptyQ :: Queue a
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a
enqueue x (Q ys) = Q (ys++[x])

firstQ :: Queue a -> a
firstQ (Q xs) = head xs

dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (tail xs)

--1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el
--final de la lista y desencolarse por delante.


