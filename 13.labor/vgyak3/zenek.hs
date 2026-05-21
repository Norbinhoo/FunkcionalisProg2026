{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Zenek where

import Data.Aeson
import GHC.Generics
import Data.Ord (comparing)
import Data.List (sortBy, sort, group, maximumBy, minimumBy)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

-- ==========================================
-- ADATSZERKEZETEK
-- ==========================================

data Dal = Dal
  { cim   :: T.Text
  , eloado :: T.Text
  , album  :: T.Text
  , hossz  :: Int
  , mufaj  :: T.Text
  , ev     :: Int
  } deriving (Show, Generic)

instance FromJSON Dal

data Dalok = Dalok
  { dalok :: [Dal]
  } deriving (Show, Generic)

instance FromJSON Dalok

-- ==========================================
-- LABOROS BEOLVASÁS
-- ==========================================
beolvasDalok = do
    jsonData <- B.readFile "13.labor/vgyak3/zenek.json"
    case decode jsonData of
        Just csomag -> return (dalok csomag)
        Nothing     -> do
            putStrLn "Hiba! A zenek.json nem olvashato vagy hibas a szerkezete!"
            return []

-- ==========================================
-- FELADATOK (Kizárólag beépített függvényekkel)
-- ==========================================

-- a. Rendezzük a dalokat hossz szerint, majd írassuk ki a képernyőre
felA = do
    lista <- beolvasDalok
    let rendezett = sortBy (comparing hossz) lista
    mapM_ (\d -> putStrLn $ T.unpack (cim d) ++ " - " ++ T.unpack (eloado d) ++ " (" ++ show (hossz d) ++ " mp)") rendezett

-- b. Egy előadó dalai és összesített játékideje (futtatás pl: felB "Queen")
felB keresettStr = do
    lista <- beolvasDalok
    let keresett = T.toLower (T.pack keresettStr)
        dalai = filter (\d -> T.toLower (eloado d) == keresett) lista
        osszMasodperc = sum (map hossz dalai)
        osszPerc = (fromIntegral osszMasodperc :: Double) / 60.0
    if null dalai
        then putStrLn "Nincs találat erre az előadóra."
        else do
            putStrLn "i. Dalok listája:"
            mapM_ (\d -> putStrLn $ "  - " ++ T.unpack (cim d) ++ " [" ++ T.unpack (album d) ++ "]") dalai
            putStrLn $ "ii. Összesített játékidő: " ++ show osszMasodperc ++ " mp (~" ++ show osszPerc ++ " perc)"

-- c. Határozzuk meg melyik műfajban van a legtöbb és a legkevesebb dal
felC = do
    lista <- beolvasDalok
    -- Gyakoriság kiszámítása teljesen beépített függvényekkel (sort -> group -> map)
    let mufajok = map mufaj lista
        stat = map (\g -> (head g, length g)) (group (sort mufajok))
        
        -- Beépített maximumBy és minimumBy a darabszám (snd) alapján
        maxDb = snd (maximumBy (comparing snd) stat)
        minDb = snd (minimumBy (comparing snd) stat)
        
        -- Holtverseny kezelése egyszerű beépített szűréssel (filter)
        legtobb = map fst (filter (\(_, db) -> db == maxDb) stat)
        legkevesebb = map fst (filter (\(_, db) -> db == minDb) stat)
        
    putStrLn $ "A legtöbb dal ebben a műfajban van (" ++ show maxDb ++ " db): " ++ T.unpack (T.intercalate ", " legtobb)
    putStrLn $ "A legkevesebb dal ebben a műfajban van (" ++ show minDb ++ " db): " ++ T.unpack (T.intercalate ", " legkevesebb)

-- d. Határozzuk meg melyik album tartalmazza a legtöbb és a legkevesebb dalt
felD = do
    lista <- beolvasDalok
    let albumok = map album lista
        stat = map (\g -> (head g, length g)) (group (sort albumok))
        
        maxDb = snd (maximumBy (comparing snd) stat)
        minDb = snd (minimumBy (comparing snd) stat)
        
        legtobb = map fst (filter (\(_, db) -> db == maxDb) stat)
        legkevesebb = map fst (filter (\(_, db) -> db == minDb) stat)
        
    putStrLn $ "A legtöbb dalt tartalmazó album(ok) (" ++ show maxDb ++ " db):"
    mapM_ (\a -> putStrLn $ "  - " ++ T.unpack a) legtobb
    putStrLn $ "A legkevesebb dalt tartalmazó album(ok) (" ++ show minDb ++ " db):"
    mapM_ (\a -> putStrLn $ "  - " ++ T.unpack a) legkevesebb

-- e. Készítsünk statisztikát arról, hogy hány zene van évtizedenként
felE = do
    lista <- beolvasDalok
    let evtizedek = map (\d -> (ev d `div` 10) * 10) lista
        stat = map (\g -> (head g, length g)) (group (sort evtizedek))
        rendezettStat = sortBy (comparing fst) stat
    mapM_ (\(evti, db) -> putStrLn $ show evti ++ "-es évek: " ++ show db ++ " db dal") rendezettStat