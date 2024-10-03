hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
--Propósito: Retorna todos los hechizos aprendidos por los magos.
--Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos edm = hechizosDeEn (magos edm) edm

hechizosDeEn :: [Nomrre] -> EscuelaDeMagia -> Set Hechizo
--Eficiencia O (M * (log M + H log H))
hechizosDeEn [] edm = emptyS
hechizosDeEn (n:ns) edm = unionS (hechizosDe n edm) (hechizosDeEn ns edm)

hayUnExperto :: EscuelaDeMagia -> Bool
--Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
--Eficiencia: O(log M)
hayUnExperto edm = let (m,e) = egresarUno edm
                    in leFaltanAprender (nombre m) e = 0


egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
--Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos magos.
Eficiencia: O(M log M)
egresarExpertos edm = if not (hayUnExperto edm)
                            then ([], edm)
                            else let (m, edmSinM) = egresarUno edm
                                     (ms, edmSinMs) = egresarExpertos edmSinM   
                                    in(m:ms, edmSinMs)