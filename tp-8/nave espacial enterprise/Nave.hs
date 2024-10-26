module Nave(Nave,)

where 

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
{-
INV:REP: Dada la estructura MkN m h t
* Un tripulante no puede estar dentro de mas de un set asociado a un sector.
* Los tripulantes se ordenan por rango de mayor a menor en la Heap 'h'.
* El sector dentro del par 't' debe ser el que mas tripulantes tiene dentro del mal map 'm' de sectores.
* El segundo elemento del par 't' debe ser igual a la cantidad de tripulantes del sector de 't'
-}

naveVacia :: [Sector] -> Nave
--Propósito: Crea una nave con todos esos sectores sin tripulantes.
--Precondición: la lista de sectores no está vacía
--Costo: O(S log S) siendo S la cantidad de sectores de la lista.
naveVacia ss = MkN (agregarSectores ss emptyM) emptyH (head ss, 0)

tripulantesDe :: Sector -> Nave -> Set Tripulante
--Propósito: Obtiene los tripulantes de un sector.
--Costo: O(log S) siendo S la cantidad de sectores.
tripulantesDe s (MkN m h t) = case lookupM s m of
                                Nothing -> emptyS
                                Just s -> s

sectores :: Nave -> [Sector]
--Propósito: Denota los sectores de la nave
--Costo: O(S) siendo S la cantidad de sectores.
sectores (MkN m h t) =  domM m

conMayorRango :: Nave -> Tripulante
--Propósito: Denota el tripulante con mayor rango.
--Precondición: la nave no está vacía.
--Costo: O(1).
conMayorRango (MkN m h t) = findMax h

conMasTripulantes :: Nave -> Sector
--Propósito: Denota el sector de la nave con más tripulantes.
--Costo: O(1).
conMasTripulantes (MkN m h t) = fst t

conRango :: Rango -> Nave -> Set Tripulante
--Propósito: Denota el conjunto de tripulantes con dicho rango.
--Costo: O(P log P) siendo P la cantidad de tripulantes.
conRango r (MkN m h t) = tripulantesDeRango r h

sectorDe :: Tripulante -> Nave -> Sector
--Propósito: Devuelve el sector en el que se encuentra un tripulante.
--Precondición: el tripulante pertenece a la nave.
--Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
sectorDe trip n = sectorConTripulante trip (sectores n) n

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
--Propósito: Agrega un tripulante a ese sector de la nave.
--Precondición: El sector está en la nave y el tripulante no.
--Costo: (log s + log t) + (log t) 

agregarTripulante t s (MkN m h t) = MkN (agregarASector t s m) (insertH t) (actualizarPar t s (sizeS (fromJust (lookupM s m)) + 1))

actualizarPar :: (Sector, Int) -> Sector -> Int -> (Sector, Int)
-- costo O(1)
actualizarPar par s n = if n > snd par
                                then (s,n)
                                else par

agregarASector :: Tripulante -> Sector -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
-- costo: log s + log t
agregarASector trip s m = case lookupM s m of
                                Nothing -> error "No existe"
                                Just ts -> assocM s (addS trip ts) m

-- ===========
--  SUBTAREAS
--- ==========

agregarSectores :: [Sector] -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
-- PROP: agrega todos los sectores de la lista sin tripulantes al map.
--costo: S log S
agregarSectores [] m     = m
agregarSectores (s:ss) m = agregarSectores ss (assocM s emptyS m)

tripulantesDeRango :: Rango -> Heap Tripulante -> Set Tripulante
tripulantesDeRango r h = if isEmptyH h
                            then emptyS 
                            let tMax = findMax h
                            in else if rango tMax = r
                                    then addS tMax (tripulantesDeRango r (deleteMax h))
                                    else tripulantesDeRango r (deleteMax h)

sectorConTripulante :: Tripulante -> [Sector] -> Nave -> Sector
-- prop: dado un tripulante 't', una lista de sectores y una nave, indica cual es el sector del tripulante t.
-- precon: el tripulante debe estar en uno de los sectores.
sectorConTripulante trip [] n = error "No hay sector donde se encuentre el tripulante"
sectorConTripulante trip (s:ss) (MkN m h t) = if belongs trip (fromJust (lookupM s n))
                                                then s
                                                else sectorConTripulante trip ss
