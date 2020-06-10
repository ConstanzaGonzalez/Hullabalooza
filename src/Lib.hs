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

tocar :: Banda -> Festival -> Festival
tocar banda festival = (genero banda) festival

hullabalooza = Festival "palermo" 20000 "indiferente" [miranda, redondos, soda, metallica]

redondos = Banda ["legendaria", "pogosa"] 45 rockNacional
miranda = Banda ["insipida", "incolora", "inodora"] 60 pop
soda = Banda ["irrepetible"] 40 rockNacional
metallica = Banda ["legendaria", "vendida"] 60 heavyMetal
trashMetalBanda = Banda ["legendaria", "vendida"] 60 trashMetal

theStrokes = Banda ["suicidio asistido", "emocional", "linda"] 45 (pop.heavyMetal)



