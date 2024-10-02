import Nave 

sectores :: Nave -> Set SectorId -- costo O(T * (S log S))
--Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados)
sectores n = sectoresConTripulantes (tripulantesN n)

sinSectoresAsignados :: Nave ->[Tripulante]
--Propósito: Devuelve los tripulantes que no poseen sectores asignados
sinSectoresAsignados n = tripulantesSinSectores (tripulantesN n)

barriles :: Nave -> [Barril] -- costo O(S * (C*2 + log S))
--Propósito: Devuelve todos los barriles de los sectores asignados de la nave
barriles n = barrilesEnSectores n (sectores n)


-- =================
--    SUBTAREAS
-- =================

sectoresConTripulantes :: [Tripulante] -> Set SectorId
-- Prop: dada una lista de tripulantes describe todos los sectores en los que hay al menos un tripulante
-- costo O(T * (S log S))
{- es el costo de todos los tripulantes de la lista
por cada union de los sets de sectores que es de costo (S log S)
-}
sectoresConTripulantes [] = emptyS
sectoresConTripulantes (t:ts) = unionS (sectoresT t) (sectoresConTripulantes ts)

tripulantesSinSectores :: [Tripulante] -> [Tripulante]
-- costo O(T)
-- se realiza sizeS (sectoresT t), una igualdad y un cons que son todas operaciones constantes por cada elemento de la lista de tripulantes
tripulantesSinSectores [] = []
tripulantesSinSectores (t:ts) = if sizeS (sectoresT t) == 0
                                    then t : tripulantesSinSectores ts
                                    else tripulantesSinSectores ts

barrilesEnSectores :: Nave -> [SectorId] -> [Barril] -- O(S * (C*2 + log s))
barrilesEnSectores n [] = []
barrilesEnSectores n [sId:ssId] = barrilesDelSector sId n ++ barrilesEnSectores n ssid

barrilesDelSector :: SectorId -> Nave -> [Barril] -- O(C*2 + log s)
barrilesDelSector sId n = barrilesEnComponentes (snd (datosDeSector sId n))

barrilesEnComponentes :: [Componente] -> [Barril] -- O(C*2)
barrilesEnComponentes [] = []
barrilesEnComponentes (c:cs) = barrilesDelComponente c ++ barrilesEnComponentes cs

barrilesDelComponente :: Componente -> [Barril] -- O(1)
barrilesDelComponente Almacen bs = bs
barrilesDelComponente _ = []


{-
Dar una posible representación para el tipo Sector, de manera de que se pueda cumplir con el orden dado para cada
operación de la interfaz, pero sin implementarlas.
Sector, siendo C la cantidad de contenedres y T la cantidad de tripulantes:

crearS :: SectorId -> Sector O(1)
sectorId :: Sector -> SectorId O(1)
componentesS :: Sector -> [Componente] O(1)
tripulantesS :: Sector -> Set Nombre O(1)
agregarC :: Componente -> Sector -> Sector O(1)
agregarT :: Nombre -> Sector -> Sector O(log T)
-}

{-
data Sector = S id [Componente] (Set Nombre)
-}