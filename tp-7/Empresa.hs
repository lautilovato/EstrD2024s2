module Empresa(Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, agregarSector, agregarEmpleado)

where 

import SetV1
import MapV1
import Empleado
type SectorId = Int
type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)
{-
INV.REP: en ConsE MapS MaPE,
* Para cada clave c en MapE, el empleado asociado tiene cuil c.
* Para cada clave S en MapS, todos los empleados en el set asociado a S, tienen a S en la lista de sectores en que trabajan.
* Ningun Cuil de las claves es negatio.
*
-}

consEmpresa :: Empresa --Costo: O(1)
--Propósito: construye una empresa vacía. 
consEmpresa = ConsE emptyM emptyM 

buscarPorCUIL :: CUIL -> Empresa -> Empleado -- Costo: O(log E)
--Propósito: devuelve el empleado con dicho CUIL.
--Precondición: el CUIL es de un empleado de la empresa.
buscarPorCUIL c (ConsE sses cses) = fromJust (lookupM c cses)

empleadosDelSector :: SectorId -> Empresa -> [Empleado] --Costo: O(log S + E)
--Propósito: indica los empleados que trabajan en un sector dado.
empleadosDelSector sId (ConsE sses cses) = setToList (fromJust (lookupM sId sses))

todosLosCUIL :: Empresa -> [CUIL] -- Costo: O(E)
--Propósito: indica todos los CUIL de empleados de la empresa.
todosLosCUIL (ConsE sses cses) = keys cses 

todosLosSectores :: Empresa -> [SectorId] -- Costo: O(S)
--Propósito: indica todos los sectores de la empresa.
todosLosSectores (ConsE sses cses) = keys sses

agregarSector :: SectorId -> Empresa -> Empresa -- Costo: O(log S)
--Propósito: agrega un sector a la empresa, inicialmente sin empleados.
agregarSector sId (ConsE sses cses) = ConsE (assocM sId emptyS sses) cses

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa -- Costo: O(S log S).
--Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el CUIL dado.
agregarEmpleado ssIds c (ConsE sses cses) = ConsE (agregarEmpleadoASectores (consEmpleado c (listToSet ssIds)) ssIds sses) 
                                                        (assocM c (consEmpleado c (listToSet ssIds)) cses)

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa -- O(log s).
--Propósito: agrega un sector al empleado con dicho CUIL.
agregarASector sId c (ConsE sses cses) = ConsE sses (assocM c (incorporarSector sId (fromJust c cses)) cses)

borrarEmpleado :: CUIL -> Empresa -> Empresa -- Costo: calcular.
--Propósito: elimina al empleado que posee dicho CUIL.
borrarEmpleado cuil (ConsE sses cses) = ConsE (eliminarEmpleadoDeSectores (fromJust(lookupM c cses)) (sectores (fromJust (lookupM c cses))) sses) (deleteM c cses)

eliminarEmpleadoDeSectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
eliminarEmpleadoDeSectores _ [] map = map
eliminarEmpleadoDeSectores e (x:xs) map = eliminarEmpleadoDeSectores e xs (eliminarDeSector e x map)

eliminarDeSector :: Empleado -> SectorId -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
eliminarDeSector e sId map = assocM sid (removeS e (fromJust (lookupM sId map))) map

--auxiliares

fromJust :: Maybe a -> a -- O(1)
fromJust (Just x) = x
fromJust Nothing = error "no existe un valor valido"

listToSet :: [a] -> Set a
listToSet []     = emptyS
listToSet (x:xs) = addS x (listToSet xs)

agregarEmpleadoASectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmpleadoASectores e [] map          = map
agregarEmpleadoASectores e (sId:ssIds) map = agregarEmpleadoASectores e ssIds (assocM sId (addS e (fromJust lookupM sId )) map)


