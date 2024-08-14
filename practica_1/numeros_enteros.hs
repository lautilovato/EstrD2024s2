-- punto 1

sucesor :: Int -> Int
sucesor n = n+1

sumar :: Int -> Int -> Int
sumar n m = n+m

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = ( div n  m , mod n m )

maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if (n > m)
                        then n
                        else m

-- punto 2

{-
De 4 ejemplos de expresiones diferentes que denoten el número 10, 
utilizando en cada expresión a todas las funciones del punto anterior.
1. maxDelPar (divisionYResto (sucesor 19) (sumar 1 1) )
2. maxDelPar (divisionYResto (sumar 15 5) (sucesor 1) )
3. maxDelPar (divisionYResto (sumar 1 9) (sucesor 0 ) )
4. maxDelPar (divisionYResto (sumar 3 7 ) (sucesor 23 ) )
-}

