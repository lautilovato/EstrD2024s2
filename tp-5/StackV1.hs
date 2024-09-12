module StackV1
(Stack ,emptyST, isEmptyS, push, top, pop, lenS )

where 

data Stack a = ST [a] Int -- elementos cantElementos
{- INV.REP.: en ST [a] n
      * n >= 0
-}

emptyST :: Stack a
emptyST = ST [] 0

isEmptyS :: Stack a -> Bool
--Dada una pila indica si está vacía.
isEmptyS (ST xs _) = null xs

push :: a -> Stack a -> Stack a
--Dados un elemento y una pila, agrega el elemento a la pila.
push x (ST ys n) = (ST (x:ys) (n+1))

top :: Stack a -> a
--Dada un pila devuelve el elemento del tope de la pila.
top (ST xs _) = head xs

pop :: Stack a -> Stack a
--Dada una pila devuelve la pila sin el primer elemento.
pop (ST xs n) = (ST(tail xs) (n-1))

lenS :: Stack a -> Int
--Da la cantidad de elementos en la pila.
lenS (ST _ n) = n