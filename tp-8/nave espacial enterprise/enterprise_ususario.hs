--Recordatorio: De existir, agregue las precondiciones en las funciones solicitadas. ¡No deje de dividir en subtareas! Y no olvide
--además incluir propósito y precondiciones de las funciones auxiliares que necesite programar.

tripulantes :: Nave -> Set Tripulante
--Propósito: Denota los tripulantes de la nave
tripulantes n = tripulantesDeSectores (sectores n) n

tripulantesDeSectores :: [Sector] -> Nave -> Set Tripulante
-- prop: denota todos los triplantes de los sectores dados dentro de la nave.
-- precon: Los sectores dados deben estar dentro de la lista
--Eficiencia: O(s * (s log s + log s))??
-- por cada sector de la nave se hacen dos operaciones una de costo s log s(union) y otra de costo log s (tripulantesDe).
tripulantesDeSectores [] n = emptyS
tripulantesDeSectores (s:ss) n = union (tripulantesDe s n) (tripulantesDeSectores ss n) -- (s log s) (log s) 
{-
--Opcional (Bonus): 
bajaDeTripulante :: Tripulante -> Nave -> Nave
--Propósito: Elimina al tripulante de la nave.
--Pista: Considere reconstruir la nave sin ese tripulante.
bajaDeTripulante t n = -}