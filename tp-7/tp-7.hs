{-Ejercicio 1
Indicar el costo de heapsort :: Ord a => [a] -> [a] (de la práctica anterior) suponiendo que
el usuario utiliza una priority queue con costos logarítmicos de inserción y borrado (o sea, usa una
Heap como tipo de representación).-}
-- O(n log n)

{-Ejercicio 1
Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los
invariantes de BST y sin elementos repetidos (despreocuparse por el hecho de que el árbol puede
desbalancearse al insertar o borrar elementos). En todos los costos, N es la cantidad de elementos
del árbol. Justificar por qué la implementación satisface los costos dados.-}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

belongsBST :: Ord a => a -> Tree a -> Bool -- Costo: O(log N)
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Cumple con el costo porque al ser un arbol ordenado solo reccore una rama del mimso
belongsBST x EmptyT = False
belongsBST x (NodeT y ti td) = if x == y
                                    then True 
                                    else if x < y 
                                        then belongsBST x ti
                                        else belongsBST x td

insertBST :: Ord a => a -> Tree a -> Tree a -- Costo: O(log N)
--Propósito: dado un BST inserta un elemento en el árbol 
--Cumple con el costo porque al ser un arbol ordenado solo reccore una rama del mimso
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x == y
                                then NodeT x ti td
                                else if x < y
                                    then NodeT y (insertBST x ti) td
                                    else NodeT y ti (insertBST x td) 

deleteBST :: Ord a => a -> Tree a -> Tree a --Costo: O(log N)
--Propósito: dado un BST borra un elemento en el árbol
--Cumple con el costo porque al ser un arbol ordenado solo reccore una rama del mimso
deleteBST x EmptyT = EmptyT
deleteBST x (NodeT y ti td) = if x == y
                                then rearmarBST ti td
                                else if x < y 
                                    then NodeT y (deleteBST x ti) td
                                    else NodeT y ti (deleteBST x td) 

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
  -- PRECOND: los dos árboles son BSTs
rearmarBST EmptyT td = td
rearmarBST ti     td = let (m, ti') = splitMaxBST ti
                        in NodeT m ti' td

splitMinBST :: Ord a => Tree a -> (a, Tree a) -- Costo: O(log N)
--Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo
splitMinBST (NodeT x EmptyT td) = (x,td)
splitMinBST (NodeT x ti td) = let (m, ti') = splitMinBST ti
                                   in (m, NodeT x ti' td)

splitMaxBST :: Ord a => Tree a -> (a, Tree a) -- Costo: O(log N)
  -- PRECOND: el árbol es BST, y NO está vacío
splitMaxBST (NodeT x ti EmptyT) = (x, ti)  
splitMaxBST (NodeT x ti td)     = let (m, td') = splitMaxBST td
                                   in (m, NodeT x ti td')


esBST :: Ord a => Tree a -> Bool --Costo: O(N2)
--Propósito: indica si el árbol cumple con los invariantes de BST. 
--Cumple con el costo porque en el peor de los casos se realiza una operacion lineal por cada elemento
esBST EmptyT = True
esBST (NodeT x ti td) = esMayorAelementoDe x ti && esMenorAelementoDe x td && esBST ti && esBST td

esMayorAelementoDe :: Ord a => a -> Tree a -> Bool
esMayorAelementoDe _ EmptyT = True
esMayorAelementoDe x (NodeT y ti td) = x > y

esMenorAelementoDe :: Ord a => a -> Tree a -> Bool
esMenorAelementoDe _ EmptyT = True
esMenorAelementoDe x (NodeT y ti td) = x < y

{-
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a --Costo: O(log N)
--Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
elMaximoMenorA x EmptyT =
elMaximoMenorA x (NodeT y ti td) = -}

{-
8. elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
elemento dado.
Costo: O(log N)
-}

balanceado :: Tree a -> Bool -- Costo: O(N2)
--Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
--nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
balanceado EmptyT = True
balanceado (NodeT x ti td) = (heightT ti) - (heightT td) >= (-1) && (heightT ti) - (heightT td) <= 1 && balanceado ti && balanceado td

heightT :: Tree a -> Int  -- Costo: O(N)
heightT EmptyT = 0
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)
