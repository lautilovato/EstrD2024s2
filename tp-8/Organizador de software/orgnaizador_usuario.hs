--Implementar las siguientes funciones como usuario del TAD Organizador, establecer su eficiencia y justificarla:

programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
--Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas programaron juntas.
--Precondición: las personas deben existir en el organizador.
--Eficiencia: (C log C) / chequear

programasEnComun p1 p2 org = intersection (programasDe org p1) (programasDe org p2)

esUnGranHacker :: Organizador -> Persona -> Bool
--Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
--Eficiencia: (C * (log P + log C) )
esUnGranHacker org p = esAutorDeLosProgramas p (todosLosProgramas org) org 

esAutorDeLosProgramas :: Persona ->  [Checksum] -> Organizador -> Bool
-- prop: dada una persona una lista de programas y un organizador, indica si esa persona es autor de todos los programas de ese organizador
--Eficiencia: (C * (log P + log C) )
{- este es el costo, porque por cada programa de la lista se realiza una operacion de costo (log P + log C).
-}
esAutorDeLosProgramas p [] org = 
esAutorDeLosProgramas p (c:cs) org = esAutorDelPrograma p c org && esAutorDeLosProgramas p cs org

esAutorDelPrograma :: Persona -> Checksums -> Organizador -> Bool -- 
-- prop: dada una persona y un programa con su organizador, indica si esa persona es autor del programa
-- Eficiencia: (log P + log C)
{- El costo es (log P + log C) porque autoresDe, es de costo log C, siendo C la cantidad de programas del organizador,
y belongs en este caso es de costo log P siendo p la cantidad de personas dentro del set. Entonces al ser recorridos sobre estructuras
distintas el costo de ambas operaciones se suma y eso da el costo final.
-}
esAutorDelPrograma p c org = belongs p (autoresDe org c)
