module RAList 
(RAList,isEmptyRAL, lengthRAL, get, minRAL, add)

where

data RAList a =  MkR Int (Map Int a) (Heap a)

emptyRAL :: RAList a -- Eficiencia: O(1).
--Propósito: devuelve una lista vacía.
emptyRAL = MkR 0 emptyM emptyH

isEmptyRAL :: RAList a -> Bool -- Eficiencia: O(1).
--Propósito: indica si la lista está vacía.
isEmptyRAL (MkR n m h) = n == 0

lengthRAL :: RAList a -> Int -- Eficiencia: O(1).
--Propósito: devuelve la cantidad de elementos.
lengthRAL (MkR n m h) = n


get :: Int -> RAList a -> a -- Eficiencia: O(log N).
--Propósito: devuelve el elemento en el índice dado.
--Precondición: el índice debe existir.
get k (MkR n m h) = if k > 0 && k < n
                        then fromJust (lookupM k m)
                        else error "no existe elemento con ese indice"

minRAL :: Ord a => RAList a -> a -- Eficiencia: O(1).
--Propósito: devuelve el mínimo elemento de la lista.
--Precondición: la lista no está vacía.
minRAL (MkR n m h) = findMin h

add :: Ord a => a -> RAList a -> RAList a -- Eficiencia: O(log N).
--Propósito: agrega un elemento al final de la lista.
add x (MkR n m h) = MkR (n+1) (assocM n x m) (insertH x h)

elems :: Ord a => RAList a -> [a]
-- Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
-- Eficiencia: O(N log N).
elems (MkR n m h ) = heapToList h


remove :: Ord a => RAList a -> RAList a -- Eficiencia: O(N log N).
--Propósito: elimina el último elemento de la lista.
--Precondición: la lista no está vacía.
remove (MkR n m h) = MkR (n-1) (deleteM (n-1) m) (sinElemento (fromJust (lookupM (n-1) m)) h)

set :: Ord a => Int -> a -> RAList a -> RAList a -- Eficiencia: O(N log N).
--Propósito: reemplaza el elemento en la posición dada.
--Precondición: el índice debe existir.
set k x (MkR n m h) = MkR n (assocM k x m) (updateHeap x (fromJust (lookupM k m)) h)


addAt :: Ord a => Int -> a -> RAList a -> RAList a -- Eficiencia: O(N log N).
--Propósito: agrega un elemento en la posición dada.
--Precondición: el índice debe estar entre 0 y la longitud de la lista.
--Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
---Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar 
--también como argumento la máxima posición posible.
addAt k x (MkR n m h) = if k > 0 && k < n
                        then MkR (n+1) (assocM k x (actualizarDesdeHasta k n m)) (insertH x h)
                        else error "no es posible agregar un elemento con ese indice"

-- =============
-- SUBTAREAS
-- =============

heapToList :: Ord a => Heap a -> [a] -- costo O(N log N).
heapToList h = if isEmptyH h 
                    then []
                    else findMin h : heapToList (deleteMin h)

sinElemento :: Ord a => a -> Heap a -> Heap a  -- O(N log N) 
-- Dado un elemento y un heap, devuelve ese heap sin el elemento
sinElemento x h = listToHeap (eliminarElemento x (heapToList h))

eliminarElemento :: a -> [a] -> [a] -- O(N)
eliminarElemento x [] = []
eliminarElemento x (y:ys) = if x == y 
                                then ys
                                else y : eliminarElemento x ys

listToHeap :: [a] -> Heap a -- O(N log N)
-- Prop : dada una lista de elementos, la lista en un heap
listToHeap [] = emptyH
listToHeap (x:xs) = insertH x (listToHeap xs)

updateHeap :: a -> a -> Heap a -> Heap a -- O(N log N)
-- PROP: dados dos elementos y un heap, reemplaza el primero al segundo dentro del heap
updateHeap x y h = insertH x (sinElemento y h)

actualizarDesdeHasta :: Int -> Int -> Map Int a -> Map Int a
--PROP: Dados dos indices y un map, acualiza las claves de ese map incrementandolas en uno desde el primer hasta el segundo indice
--PRECOND: el primer indice debe ser menor o igual al segundo    
actualizarDesdeHasta x y m = if x == y
                                then assocM (x+1) (fromJust (lookupM x m)) m
                                else actualizarDesdeHasta (x+1) y (assocM (x+1) (fromJust (lookupM x m)) m) 