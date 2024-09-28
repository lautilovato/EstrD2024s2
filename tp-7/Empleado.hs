module Empleado(Empleado, consEmpleado, cuil, incorporarSector, sectores)

where

import SetV1

type CUIL       = Int
type SectorId   = Int

data Empleado = E CUIL (Set SectorId)  deriving (Eq, Ord)--  Cuil Sectores

consEmpleado :: CUIL -> Empleado -- Costo: O(1)
-- Propósito: construye un empleado con dicho CUIL
consEmpleado c = E c emptyS

cuil :: Empleado -> CUIL -- Costo: O(1)
--Propósito: indica el CUIL de un empleado.
cuil (E c sIds) = c

incorporarSector :: SectorId -> Empleado -> Empleado --Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
--Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado. 
incorporarSector sId (E c sIds) = E c (addS sId sIds)

sectores :: Empleado -> [SectorId] -- Costo: O(S)
--Propósito: indica los sectores en los que el empleado trabaja.
sectores (E c sIds) = setToList sIds