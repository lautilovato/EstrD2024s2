import SetV1

import QueueV1
-- ==============================
-- 1. Cálculo de costos
-- ==============================
-- Especicar el costo operacional de las siguientes funciones:

head' :: [a] -> a
head' (x:xs) = x
--Constante

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
--Constante

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
-- Lineal

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
-- Lineal

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
-- Cuadratico

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs
-- Lineal

{-
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                            then sinRepetidos xs
                            else x : sinRepetidos xs
-- Cuadratico
-}

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
-- Lineal

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
-- Cuadratico

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs
-- Lineal

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs
-- Lineal

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)
-- Cuadratico

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
--Lineal

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs
--Lineal

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs = let m = minimo xs
                in m : ordenar (sacar m xs)
--Cuadratico


-- ==============================
--2. Set (conjunto)
-- ==============================
--Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

-- (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen [] s = []
losQuePertenecen (y:ys) s = if belongs y s
                                then y : losQuePertenecen ys s
                                else losQuePertenecen ys s

set1 = addS 2 (addS 1 emptyS)
set2 = addS 4 (addS 3 set1)
set3 = addS 1 (addS 10 emptyS)

sinRepetidos :: Eq a => [a] -> [a]
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar
sinRepetidos xs = setToList (agregarLista xs emptyS)

agregarLista :: Eq a => [a] -> Set a -> Set a
agregarLista [] s = s
agregarLista (x:xs) s = unionS (addS x s) (agregarLista xs s)

unirTodos :: Eq a => Tree (Set a) -> Set a
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos EmptyT = emptyS
unirTodos (NodeT x t1 t2) = unionS x (unionS (unirTodos t1) (unirTodos t2))

treeSet = (NodeT set1 (NodeT set2 EmptyT EmptyT) (NodeT set2 EmptyT (NodeT set3 EmptyT EmptyT)))

-- ==============================
--3. Queue (cola)
-- ==============================

-- INTERFAZ (Queue,emptyQ, isEmptyQ, enqueue, firstQ, dequeue )

lengthQ :: Queue a -> Int
--Cuenta la cantidad de elementos de la cola.
lengthQ q = if isEmptyQ q
                then 0
                else 1 + lengthQ (dequeue q)

queueToList :: Queue a -> [a]
-- Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
queueToList q = if isEmptyQ q
                    then []
                    else (firstQ q) : queueToList (dequeue q)

unionQ :: Queue a -> Queue a -> Queue a
--Inserta todos los elementos de la segunda cola en la primera
unionQ q1 q2 = if isEmptyQ q2
                    then q1
                    else unionQ (enqueue (firstQ q2) q1) (dequeue q2)

q1 = enqueue 1 (enqueue 2 (enqueue 3 (emptyQ)))
q2 = enqueue 1 (enqueue 4 (enqueue 5 (emptyQ)))