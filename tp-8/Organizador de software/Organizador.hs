module Organizador
(Organizador, )

where 
    
data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
{-
INV:REP: dada la estructura MkO mCP mPC
* Todas las personas que esten dentro de un set relacionado con un Checksum en mCP deben estar en mPC como clave y deben tener ese 
chekcsum dentro de su set de valores y viceversa
-}
nuevo :: Organizador
--Propósito: Un organizador vacío.
--Eficiencia: O(1)
-- en este caso emptyM al ser una operacion constante y repetirse dos veces, el costo de nuevo tambien lo es.
nuevo = MkO emptyM emptyM

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
{-Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
de dicho programa.
Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
no está vacío.
Eficiencia: O(log C + (P *(log P + log C)) ).-}
{-
Este costo es porque la operacion del primer map (assocM) es de costo log C y la del segundo (agregarProgramasaPs)
es de costo (P *(log P + log C)).
-}
agregarPrograma (MkO m1 m2) ch ps = MkO (assocM ch ps m1) (agregarProgramaAPs c (set2list ps) m2)

todosLosProgramas :: Organizador -> [Checksum]
--Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
--Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
-- el costo es O(C) porque domM es de costo O(C)
todosLosProgramas (MkO m1 m2) = domM m1

autoresDe :: Organizador -> Checksum -> Set Persona
--Propósito: denota el conjunto de autores que aparecen en un programa determinado.
--Precondición: el Checksum debe corresponder a un programa del organizador.
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
-- Tiene este costo porque lookupM es de costo O(log C)
autoresDe (MkO m1 m2) c = fromJust (lookupM m1 c)

programasDe :: Organizador -> Persona -> Set Checksum
--Propósito: denota el conjunto de programas en los que participó una determinada persona.
--Precondición: la persona debe existir en el organizador.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
-- tiene este costo porqub lookupM es de costo O(log P)
programasDe (MkO m1 m2) p = fromJust (lookupM m2 p)

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
--Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
--Precondición: las personas deben ser distintas.
--Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
--programas del organizador, y C la cantidad total de programas.
-- tiene este costo ya que las operaciones lookupM son de costo log P e intersection es de costo O(C log C)
programaronJuntas (MkO m1 m2) p1 p2 = not isEmptyS (intersection (fromJust (lookupM m2 p1)) (fromJust(lookupM m2 p2)) )

nroProgramasDePersona :: Organizador -> Persona -> Int
--Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.
-- tiene este costo porque lookupM es de costo (log P) y sizeS y fromJust son de costo O(1) por lo tanto el costo final es (log P)
nroProgramasDePersona (MkO m1 m2) p = sizeS (fromJust (lookupM m2 p))



-- ================ 
--    SUBTAREAS
-- ================

agregarProgramaAPs :: Checksum -> [Persona] -> (Map Persona (Set Checksum)) -> (Map Persona (Set Checksum))
{-prop: dado un programa , una lista de personas, y un map de personas con sets de programas, le agrega a los sets relacionados 
con las personas el programa dado.-}
-- precond: las personas de la lista deben ser una clave del map
--Eficiencia: (P *(log P + log C))
{-este costo es porque por cada persona de la lista se hace una operacion de costo (log P + log C).
-}
agregarProgramaAPs ch [] m = m
agregarProgramaAPs ch (p:ps) m = agregarProgramaAPs ch ps (agregarProgramaA ch p m)

agregarProgramaA :: Checksum -> Persona -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
-- prop: dada un programa, una person y un map de personas a sets de programas, le asigna al set de la persona dada el programa
-- precon: la persona debe ser una clave del map.
-- Eficiencia: (log P + log C)
{-
siendo p la cantidad de personas y C la cantidad de programas, este costo es porque lookupM y assocM tienen costo log P
y adds en este caso es de costo log , y al ser recorridos sobre estructuras distintas el costo se suma, y eso da el costo final.
-}
agregarProgramaA ch p m = case lookupM p m of
                            Nothing -> error "no existe"
                            Just cs -> assocM p (addS ch cs) m
