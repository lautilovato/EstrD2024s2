data Ingrediente = Salsa | Queso | Aceitunas Int | Anchoas | Anana | Roquefort
    deriving Show
data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show
pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8) (Capa Queso (Capa Salsa Prepizza))
pizza4 = Capa Anana (Capa Queso Prepizza)
pizza5 = Capa Queso (Capa Queso (Capa Roquefort (Capa Queso Prepizza)))


cantQueso :: Pizza -> Int
cantQueso Prepizza = 0
cantQueso (Capa ing p) = unoSiCeroSino (esElMismoIngrediente ing Queso) + cantQueso p

unoSiCeroSino:: Bool -> Int
unoSiCeroSino b = if(b)
                    then 1
                    else 0

esElMismoIngrediente :: Ingrediente -> Ingrediente -> Bool
esElMismoIngrediente Queso Queso = True
esElMismoIngrediente Salsa Salsa = True
esElMismoIngrediente (Aceitunas _) (Aceitunas _) = True
esElMismoIngrediente Anchoas Anchoas = True
esElMismoIngrediente Anana Anana = True
esElMismoIngrediente Roquefort Roquefort = True
esElMismoIngrediente _ _ = False 

cantAceitunas :: Pizza -> Int
cantAceitunas Prepizza = 0
cantAceitunas (Capa ing p) = aceitunasDelIngrediente ing + cantAceitunas p

aceitunasDelIngrediente :: Ingrediente -> Int
aceitunasDelIngrediente (Aceitunas n) = n
aceitunasDelIngrediente _ = 0

agregados :: Pizza -> [Ingrediente]
-- devuelve los ingredientyes de una pizza que no sean ni queso ni salsa 
agregados Prepizza = []
agregados (Capa ing p) = singularSi ing (esAgregado ing) ++ agregados p

singularSi :: a -> Bool -> [a]
singularSi x True = x:[]
singularSi _ False = []

esAgregado :: Ingrediente -> Bool
esAgregado Queso = False
esAgregado Salsa = False
esAgregado _ = True

duplicarQueso :: Pizza -> Pizza
duplicarQueso Prepizza = Prepizza
duplicarQueso (Capa ing p) =  if esElMismoIngrediente ing Queso
                                    then (Capa ing (Capa ing (duplicarQueso p)))
                                    else (Capa ing (duplicarQueso p))


data Objeto = Armadura | Escudo | Maza | Oro
    deriving Show

data Dungeon = Armario| Habitacion Objeto Dungeon Dungeon
    deriving Show

d5 = Habitacion Escudo
 (Habitacion Maza Armario
    (Habitacion Oro Armario Armario)
 )
 (Habitacion Armadura
    (Habitacion Oro Armario
        (Habitacion Escudo Armario Armario))
    (Habitacion Maza Armario
        (Habitacion Escudo Armario
            (Habitacion Oro Armario Armario)))
 )

cantidadDeOro :: Dungeon -> Int
cantidadDeOro Armario = 0
cantidadDeOro (Habitacion obj d1 d2) = unoSiCeroSino (esOro obj) + cantidadDeOro d1 + cantidadDeOro d2

esOro :: Objeto -> Bool
esOro Oro = True
esOro _ = False


profundidad :: Dungeon -> Int
profundidad Armario = 0
profundidad (Habitacion obj d1 d2) = 1 + elMasGrande (profundidad d1) (profundidad d2)

elMasGrande :: Int -> Int -> Int
elMasGrande n m = if n > m
                        then n
                        else m


cambiarMazasPorOro :: Dungeon -> Dungeon
cambiarMazasPorOro Armario = Armario
cambiarMazasPorOro (Habitacion obj d1 d2) =  (Habitacion (cambiarUnaMazaPorUnOro obj) (cambiarMazasPorOro d1) (cambiarMazasPorOro d2))

cambiarUnaMazaPorUnOro :: Objeto -> Objeto
cambiarUnaMazaPorUnOro Maza = Oro
cambiarUnaMazaPorUnOro o = o 

objsDelCaminoMasLargo :: Dungeon -> [Objeto]
objsDelCaminoMasLargo Armario = []
objsDelCaminoMasLargo (Habitacion obj d1 d2) = obj : elegirLaMasLarga (objsDelCaminoMasLargo d1) (objsDelCaminoMasLargo d2)

elegirLaMasLarga :: [Objeto] -> [Objeto] -> [Objeto]
elegirLaMasLarga os1 os2 = if length os1 > length os2
                                then os1
                                else os2