module Nave
(Nave,construir, ingresarT, sectoresAsignados, datosDeSector)

where 

data Nave = = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

-- T la cantidad de tripulantes y S la cantidad de sectores

construir :: [SectorId] -> Nave -- Eficiencia: O(S)
--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
construir ss = agregarSectoresA ss (N emptyM emptyM emptyH)


ingresarT :: Nombre -> Rango -> Nave -> Nave -- Eficiencia: O(log T)
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
ingresarT nom r (N ms mnt mht) = N ms (assocM n (crearT n r) mnt) (insertH (crearT n r) mht) -- assoc e insertH es costo (log T)


sectoresAsignados :: Nombre -> Nave -> Set SectorId -- Eficiencia: O(log T)
--Propósito: Devuelve los sectores asignados a un tripulante.
--Precondición: Existe un tripulante con dicho nombre.
sectoresAsignados nom (N ms mnt mht) = sectoresT (fromJust (lookupM nom mnt)) -- lookupM es de costo O(log T)

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente]) -- Eficiencia: O(log S)
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
datosDeSector sId (N ms mnt mht) = (tripulantesS (fromJust (lookupM sId ms)), componentesS (fromJust (lookupM sId ms))) -- lookupM es de costo O(log S), componentesS y tripulantesS O(1)

tripulantesN :: Nave -> [Tripulante] -- Eficiencia: O(log T)
--Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
tripulantesN (N ms mnt mht) = heapToList mht -- es de costo O(T log T) / chequear

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Asigna una lista de componentes a un sector de la nave.
--Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector [] sId n =  n
agregarASector (c:cs) sId (N ms mnt mht) = agregarASector cs sId (N (assocM sId (agregarC c (fromJust (lookupM sId ms)))) mnt mht)

asignarASector :: Nombre -> SectorId -> Nave -> Nave -- Eficiencia: O(log S + log T + T log T)
--Propósito: Asigna un sector a un tripulante.
--Nota: No importa si el tripulante ya tiene asignado dicho sector.
--Precondición: El tripulante y el sector existen.
asignarASector nom sId (N ms mnt mht) = N (asignarTripulanteASector nom sId ms) 
                                          (asignarSectorATripulante sId nom mnt)
                                          (updateTripulante nom sId mht)

-- =================
--    SUBTAREAS
-- =================

agregarSectoresA :: [SectorId] -> Nave -> Nave -- Eficiencia: O(S)
agregarSectoresA [] (N ms mnt mht) = N ms mnt mht
agregarSectoresA (sId:ssId) (N ms mnt mht) = agregarSectoresA ss (N (assocM sId (crearS sId) ms) mnt mht)

heapToList :: MaxHeap Tripulante -> [Tripulante]
heapToList mht = if isEmptyH mht
                        then []
                        else (maxH mht) : heapToList (deleteMaxH mht)

asignarTripulanteASector :: Nombre -> SectorId -> Map SectorId Sector -> Map SectorId Sector
-- EFICIENCIA: O(Log T + Log S)
asignarTripulanteASector nom sId m = case lookupM sId m of
                                  Nothing -> error "No existe"
                                  Just  s -> assocM sId (agregarT nom s) m 

asignarSectorATripulante :: SectorId -> Nombre -> Map Nombre Tripulante -> Map Nombre Tripulant
-- EFICIENCIA: O(Log T + Log S)
asignarSectorATripulante sId nom m = case lookupM nom m of
                                        Nothing -> error "No existe"
                                        Just t  -> assocM nom (asignarS sId t)

updateTripulante :: Nombre -> SectorId -> MaxHeap Tripulante -> MaxHeap Tripulante
-- Prop : le agrega un nuevo sector al tripulante dentro del heap
-- EFICIENCIA: O(T Log T)
updateTripulante nom sId mh = let trip = maxH mh
                                in if nombre trip == nom
                                    then insertH (asignarS sId trip) (deleteMaxH mh)
                                    else insertH trip (updateTripulante nom sId (deleteMaxH mh))