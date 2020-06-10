module Lib where
import Text.Show.Functions

laVerdad = True

data Festival = Festival {
    lugar :: String,
    publico :: Float,
    animo :: String,
    bandas :: [Banda]
} deriving(Show)

data Banda = Banda {
    descripciones :: [String],
    decibeles :: Int,
    genero :: Genero
} deriving(Show)

type Genero = Festival -> Festival

rockNacional :: Genero
rockNacional festival = aumentarPublico 100 festival

pop :: Genero
pop festival 
    | animo festival == "indiferente" = (aumentarPublico (publico festival) . cambiarAnimo "euforico") festival
    | otherwise = festival

aumentarPublico :: Float -> Festival -> Festival
aumentarPublico cantidad festival = festival { publico = publico festival + cantidad }

cambiarAnimo :: String -> Festival -> Festival
cambiarAnimo animo festival = festival { animo =  animo}

metal :: String -> Genero
metal animo festival = (aumentarPublico (0.01 * publico festival) . cambiarAnimo animo) festival

heavyMetal :: Festival -> Festival
heavyMetal festival = metal "pesado" festival

trashMetal :: Festival -> Festival
trashMetal festival = metal "basura" festival

tocar :: Festival -> Banda -> Festival
tocar festival banda = (genero banda) festival

hullabalooza = Festival "palermo" 20000 "indiferente" [miranda, redondos, soda, metallica]

redondos = Banda ["legendaria", "pogosa"] 45 rockNacional
miranda = Banda ["insipida", "incolora", "inodora"] 60 pop
soda = Banda ["irrepetible"] 40 rockNacional
metallica = Banda ["legendaria", "vendida"] 60 heavyMetal
trashMetalBanda = Banda ["legendaria", "vendida"] 60 trashMetal

theStrokes = Banda ["suicidio asistido", "emocional", "linda"] 45 (pop.heavyMetal)

suceder :: Festival -> Festival
suceder festival = foldl tocar festival (bandas festival)

type Clasificacion = Banda -> Bool

vendida :: Clasificacion
vendida banda = length (descripciones banda) >= 3 || descriptaComo "vendida" banda

acustica :: Clasificacion
acustica = (>55).decibeles

legendaria :: Clasificacion
legendaria banda = descriptaComo "legendaria" banda && decibeles banda > 40 

descriptaComo :: String -> Banda -> Bool
descriptaComo descripcion banda = elem descripcion (descripciones banda)

popularidad :: Banda -> [Clasificacion] -> Int
popularidad banda = (*100) . length . filter (perteneceAClasificacion banda)

perteneceAClasificacion :: Banda -> Clasificacion -> Bool
perteneceAClasificacion banda clasificacion = clasificacion banda 

hullabalooza2 = Festival "palermo" 20000 "indiferente" [metallica, redondos]

buenFest :: Festival -> [Clasificacion] -> Bool
buenFest festival clasificaciones = (bandaEsMasPopularQueLaAnterior clasificaciones festival)  && (popularidadAcumulada clasificaciones festival) > 1000

popularidadAcumulada :: [Clasificacion] -> Festival -> Int
popularidadAcumulada clasificaciones = sum . map (flip popularidad clasificaciones) . bandas

bandaEsMasPopularQueLaAnterior :: [Clasificacion] -> Festival -> Bool
bandaEsMasPopularQueLaAnterior clasificaciones festival = esMasPopular (map (flip popularidad clasificaciones) (bandas festival))

esMasPopular :: [Int] -> Bool
esMasPopular [] = False
esMasPopular [x] = True
esMasPopular (cabeza1:cabeza2:cola) 
        | cabeza1 < cabeza2 = esMasPopular (cabeza2:cola)
        | otherwise = False

--bandaEsMasPopularQueLaAnterior :: [Clasificacion] -> Festival -> Bool
--bandaEsMasPopularQueLaAnterior clasificaciones festival = foldr1 funcion semilla lista . map (flip popularidad lista) bandas

--[banda1, banda2, banda3]   clasificacion =  [acustica, legendaria, vendida]

--popularidad banda1 clasificacion = 100 

--foldl esMasPopularQue [300, 200, 100]
--esMasPopularQue bool valor100 valor200   
  --      | valor200 > valor100 = True
    --    |otherwise 


--8) En pop , metal, thestrokes, popularidad, buenFest
-- Permite no repetir codigo, pasarle parametros a la funciones de una forma declarativa. 
--Para llamar a funciones que reciben dos parametros, pero como ya conocemos uno de sus parametros, se lo pasamos
--parcialmente y evitamos tener que declararlo y poner los valores que recibe

--9)  Para las lista bandas no seria posible ya que no terminarian de hacer el foldl o el sum nunca
-- Para clasificacoines tampoco por el mismo caso de arriba