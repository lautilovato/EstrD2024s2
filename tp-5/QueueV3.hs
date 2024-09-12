module QueueV3
(Queue,emptyQ, isEmptyQ)

where 

data Queue a = Q [a] [a] -- fs(front stack) bk(back stack)
{- INV.REP.: en ST [a] [a]
      * Si fs se encuentra vacía, entonces la cola se encuentra vacía
-}

emptyQ :: Queue a
emptyQ = Q [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs _) = null xs



