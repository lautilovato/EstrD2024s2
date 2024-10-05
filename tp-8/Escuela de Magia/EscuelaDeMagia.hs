module EscuelaDeMagia (EscuelaDeMagia, )

where

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)
{-
INV.REP: dada la estrructura EDM hs nm ms
* todos las claves de nombres de magos en 'nm' deben corresponder a un mago con dicho nombre 
* todos los magos de nm deben estar en ms y viceversa
* un mago solo sabe hechizos que esten dentro de hs
-}

fundarEscuela :: EscuelaDeMagia
--Propósito: Devuelve una escuela vacía.
--Eficiencia: O(1)
-- tiene costo O(1) ya que las operaciones emptyS emptyM emptyPQ son todas de costo constante.
fundarEscuelas = EDM emptyS emptyM emptyPQ

estaVacia :: EscuelaDeMagia -> Bool
--Propósito: Indica si la escuela está vacía.
--Eficiencia: O(1)
-- Es de costo O(1) porque isEmptyPQ es de coso O(1)
estaVacia (EDM hs nm ms) = isEmptyPQ ms

registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
--Eficiencia: O(log M) / chequear
-- es de costo (log N + log M), ya que las operaciones lookupM y assoc son de costo log N y insertPQ es de costo log M
registrar n (EDM hs nm ms) = if lookupM n nm != Nothing
                                then EDM hs nm ms 
                                else EDM hs (assocM n (crearM n) nm) (insertPQ (crearM n) ms)

magos :: EscuelaDeMagia -> [Nombre]
--Propósito: Devuelve los nombres de los magos registrados en la escuela.
--Eficiencia: O(M)
-- el costo es es O(N) ya que domM en este caso es de costo O(N)
magos (EDM hs nm ms) = domM nm 

hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
--Propósito: Devuelve los hechizos que conoce un mago dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
--es de costo O(log N) ya que lookiupM es de costo (log N) y las otras operaciones son constantes, entonces ese es el costo final.
hechizosDe n (EDM hs nm ms) = hechizos (fromJust (lookupM n nm))


leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
--Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
-- el costo es O(log N) debido a que la operacion lookupM es de costo (log N) y todas las otras son de costo O(1)
leFaltanAprender n (EDM hs nm ms) = (sizeS hs) - sizeS (hechizos (fromJust (lookupM n nm)))

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
--Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
--Precondición: Hay al menos un mago.
--Eficiencia: O(log M)
-- el costo es O(log N + log M) ya que maxPQ y deleteMaxPQ son de costo (log M) y deleteM en este caso es de costo O(log N)
-- y al ser estructura distintas por eso el costo se suma.
egresarUno (EDM hs nm ms) = let m = maxPQ ms
                            in (m, (EDM hs (deleteM (nombre m) nm) (deleteMaxPQ ms)))

enseniar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
--Nota: No importa si el mago ya conoce el hechizo dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(M log M + log H) / me dió: O ((M log M) + (log N + log H))  

enseniar h n (EDM hs nm ms) = if belongsS h hs
                                    then EDM hs (enseniarA h n nm) (updatePQ h n ms)
                                    else EDM (addS S hs) (enseniarA h n nm) (updatePQ h (fromJust (lookiupM n nm)) ms)
                                
enseniarA :: Hechizo -> Nombre -> Map Nombre Mago -> Map Nombre Mago
-- prop: dado un hechizo un nombre y un map de nombres a magos le se devuele el map con el mago ya apreneidno el hechizo dado.
-- Eficiencia: (log N + log H)
enseniarA h n m = case lookupM n m of
                            Nothing -> error "no existe"
                            Just mago -> assocM n (aprender h magp) m

updatePQ :: Hechizo -> Mago -> PriorityQueue Mago -> PriorityQueue Mago
-- prop: dado un hechizo, un mago y una PQ de magos, el mago aprende el hechizo dado.
-- precon: el mago esta dentro de la PQ.
-- Eficiencia: O(M log M)
updatePQ h m pq = let magoMax = maxPQ pq
                    in if m == magoMax
                            then insertPQ (aprender h m) (deleteMaxPQ pq)
                            else insertPQ magoMax (updatePQ h m (deleteMaxPQ pq))
