module QueueV3
(Queue,emptyQ, isEmptyQ, enqueue, firstQ, dequeue)

where 

data Queue a = Q [a] [a] -- fs(front stack) bk(back stack)
{- INV.REP.: en ST [a] [a]
      * Si fs se encuentra vacía, entonces la cola se encuentra vacía
-}

emptyQ :: Queue a
emptyQ = Q [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs _) = null xs

enqueue :: a -> Queue a -> Queue a
enqueue x (Q fs bs) = if null fs
                              then Q (x:fs) bs
                              else Q fs (x:bs)

firstQ :: Queue a -> a
firstQ (Q fs _) = head fs

dequeue :: Queue a -> Queue a
dequeue (Q fs bs) = if null fs 
                        then Q [] []
                        else if null (tail fs)
                              then Q (reverse bs) []
                              then Q (tail fs) bs
