{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Jugador = Jugador {
  nombre    :: String,
  padre     :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador    :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = Jugador "Bart" "Homero" (Habilidad 25 60)
todd = Jugador "Todd" "Ned" (Habilidad 15 80)
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
  velocidad :: Number,
  precision :: Number,
  altura    :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = x `elem` [n .. m]
maximoSegun :: Ord a1 => (a2 -> a1) -> [a2] -> a2
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- Punto 1
-- A
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = Tiro {
    velocidad = 10, precision = (* 2) . precisionJugador $ habilidad, altura = 0
}

madera :: Palo
madera habilidad = Tiro {
    velocidad = 100, precision = (/2) . precisionJugador $ habilidad, altura = 5
}

hierro :: Number -> Palo
hierro n habilidad = Tiro {
    velocidad = (* n) . fuerzaJugador $ habilidad,
    precision = (`div` n) . precisionJugador $ habilidad,
    altura = max 0 (n - 3)
}
-- B
-- Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.
palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

-- Punto 2
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo . habilidad $ jugador

-- Punto 3
type Obstaculo = Tiro -> Tiro

esTiroNulo :: Tiro -> Tiro
esTiroNulo tiro = Tiro 0 0 0


-- A
estunelConRampita :: Tiro -> Bool
estunelConRampita tiro = ((> 90) . precision) tiro && tiroRasDelSuelo tiro

efectoSalidaTunel :: Obstaculo
efectoSalidaTunel tiro = Tiro {
    velocidad = ((*2) . velocidad) tiro, precision = 100, altura = 0
}
tiroRasDelSuelo :: Tiro -> Bool
tiroRasDelSuelo = (== 0) . altura

superatunelConRampita :: Obstaculo
superatunelConRampita tiro
  | estunelConRampita tiro && tiroRasDelSuelo tiro = efectoSalidaTunel tiro
  | otherwise = esTiroNulo tiro

-- > superaTunelConRampita (Tiro 100 100 0)

-- B
eslaguna :: Tiro -> Bool
eslaguna tiro = ((> 80) . velocidad) tiro && altura1a5Metros tiro

altura1a5Metros :: Tiro -> Bool
altura1a5Metros = between 1 5 . altura

laguna :: Number -> Obstaculo
laguna largoLaguna tiro
  | eslaguna tiro && altura1a5Metros tiro = tiro { altura = (`div` largoLaguna) . altura $ tiro }
  | otherwise =  esTiroNulo tiro

-- > superaLaguna 100 (Tiro 100 100 3)

-- C
hoyo :: Obstaculo
hoyo tiro
  | eshoyo tiro = esTiroNulo tiro
  | otherwise = esTiroNulo tiro

eshoyo :: Tiro -> Bool
eshoyo tiro = (between 5 20 . velocidad) tiro
            && tiroRasDelSuelo tiro
            && ((> 95) . precision) tiro

-- > superaHoyo (Tiro 100 100 0)

-- Punto 4
-- A
superaObstaculo :: Obstaculo -> Tiro -> Bool
superaObstaculo obstaculo tiro = (/= esTiroNulo tiro) . obstaculo $ tiro

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (superaObstaculo obstaculo . golpe jugador) palos

-- B
cuantosObstaculosSupera :: [Obstaculo] -> Tiro -> Number
cuantosObstaculosSupera obstaculos tiro = length . takeWhile (flip superaObstaculo tiro) $ obstaculos

-- C
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (cuantosObstaculosSupera obstaculos . golpe jugador) palos

-- Punto 5
padresPerdedores :: [(Jugador, Puntos)] -> [String]
padresPerdedores jugadores = map (padre . fst) (filter (not . ganoTorneo jugadores) jugadores)

ganoTorneo :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
ganoTorneo jugadores (jugador, puntos) = all ((<= puntos) . snd) (filter ((/= jugador) . fst) jugadores)