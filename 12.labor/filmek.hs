
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson (FromJSON, decode)
import Data.List (maximumBy, sortBy, nub, group, sort)
import Data.Ord (comparing)
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import System.IO

-- ============================================================================
-- ADATSZERKEZETEK (REKORD TÍPUSOK)
-- ============================================================================

data Film = Film
  { cim       :: String,
    rendezo   :: String,
    mufaj     :: String,
    ev        :: Int,
    ertekeles :: Double,
    szineszek :: [String]
  } deriving (Show, Generic)

-- A JSON fájl gyökerében lévő "filmek" kulcs leképezéséhez
data Filmek = Filmek
  { filmek :: [Film]
  } deriving (Show, Generic)

-- Az Aeson automatikusan összeköti a JSON mezőket a rekord mezőivel
instance FromJSON Film
instance FromJSON Filmek

-- ============================================================================
-- SEGÉDFÜGGVÉNY: BIZTONSÁGOS JSON BEOLVASÁS
-- ============================================================================

beolvasFilmek :: IO [Film]
beolvasFilmek = do
  jsonData <- B.readFile "12.labor/filmek.json"
  case decode jsonData of
    Just csomag -> return (filmek csomag)
    Nothing     -> do
      putStrLn "Hiba! A filmek.json nem olvashato vagy hibas a szerkezete!"
      return []

-- Segédfüggvény: Egy listában megkeresi a leggyakrabban előforduló elemet
leggyakoribb :: (Ord a) => [a] -> (a, Int)
leggyakoribb lista = 
  let csoportositott = map (\xs -> (head xs, length xs)) (group (sort lista))
  in maximumBy (comparing snd) csoportositott

-- ============================================================================
-- FELADATOK FÜGGVÉNYEI (EGYENKÉNT FUTTATHATÓK)
-- ============================================================================

-- a. feladat: Rendezés év szerint és szép kiíratás
feladatA :: IO ()
feladatA = do
  filmekLista <- beolvasFilmek
  let rendezett = sortBy (comparing ev) filmekLista
  putStrLn "\n--- Filmek ev szerint rendezve ---"
  mapM_ (\f -> putStrLn $ show (ev f) ++ ": " ++ cim f ++ " | Rendezte: " ++ rendezo f ++ " | Ertekeles: " ++ show (ertekeles f)) rendezett

-- b. feladat: Műfaj beolvasása és statisztikák (i, ii, iii pontok)
feladatB :: IO ()
feladatB = do
  filmekLista <- beolvasFilmek
  putStr "Kerem adjon meg egy mufajt (pl. Sci-Fi, Action): "
  hFlush stdout
  keresettMufaj <- getLine
  
  let szurt = filter (\f -> mufaj f == keresettMufaj) filmekLista
  
  if null szurt
    then putStrLn "Nincs ilyen mufaju film az adatbazisban!"
    else do
      -- i. pont: Kilistázás a kért formátumban
      putStrLn $ "\nA " ++ keresettMufaj ++ " mufaju filmek:"
      mapM_ (\f -> putStrLn $ cim f ++ " (" ++ show (ev f) ++ ") rendezte " ++ rendezo f) szurt
      
      -- ii. pont: Átlagertekelés
      let atlag = sum (map ertekeles szurt) / fromIntegral (length szurt)
      putStrLn $ "\nA " ++ keresettMufaj ++ " atlag ertekelese " ++ show atlag ++ "."
      
      -- iii. pont: Legjobb film a műfajban
      let legjobb = maximumBy (comparing ertekeles) szurt
      putStrLn $ "\nA " ++ keresettMufaj ++ " mufaj legjobban ertekelt muve " ++ cim legjobb ++ 
               " (" ++ show (ev legjobb) ++ "), amit " ++ rendezo legjobb ++ " rendezett, az ertekeles amit kapott " ++ show (ertekeles legjobb) ++ "."

-- c. feladat: Ki rendezte a legtöbb filmet
feladatC :: IO ()
feladatC = do
  filmekLista <- beolvasFilmek
  let rendezokListaja = map rendezo filmekLista
      (legjobbRendezo, darab) = leggyakoribb rendezokListaja
  putStrLn $ "\nA legtobbet foglalkoztatott rendezo: " ++ legjobbRendezo ++ " (" ++ show darab ++ " film)."

-- d. feladat: Melyik színész szerepel a legtöbb filmben
feladatD :: IO ()
feladatD = do
  filmekLista <- beolvasFilmek
  let osszesSzinesz = concatMap szineszek filmekLista
      (legnepszerubbSzinesz, darab) = leggyakoribb osszesSzinesz
  putStrLn $ "\nA legtobb filmben szereplo szinesz: " ++ legnepszerubbSzinesz ++ " (" ++ show darab ++ " filmben jatszott)."

-- e. feladat: Melyik rendező dolgozott a legtöbb KÜLÖNBÖZŐ színésszel
feladatE :: IO ()
feladatE = do
  filmekLista <- beolvasFilmek
  let rendezok = nub (map rendezo filmekLista)
      -- Minden rendezőhöz kigyűjtjük az egyedi színészeit
      rendezoSzineszei = map (\r -> 
        let rendezoFilmjei = filter (\f -> rendezo f == r) filmekLista
            szineszekEgyedi = nub (concatMap szineszek rendezoFilmjei)
        in (r, length szineszekEgyedi)
        ) rendezok
      (gyoztesRendezo, szineszekSzama) = maximumBy (comparing snd) rendezoSzineszei
  putStrLn $ "\nA legtobb szinesszel dolgozo rendezo: " ++ gyoztesRendezo ++ " (osszesen " ++ show szineszekSzama ++ " kulonbozo szinesz)."

-- f. feladat: top_filmek.txt mentése (értékelés >= 9.0)
feladatF :: IO ()
feladatF = do
  filmekLista <- beolvasFilmek
  let topFilmek = filter (\f -> ertekeles f >= 9.0) filmekLista
      kimenet = unlines (map (\f -> cim f ++ " (" ++ show (ev f) ++ ") - " ++ show (ertekeles f)) topFilmek)
  writeFile "12.labor/top_filmek.txt" kimenet
  putStrLn "\ntop_filmek.txt sikeresen elmentve!"

-- g. feladat: rendezok.txt mentése statisztikákkal
feladatG :: IO ()
feladatG = do
  filmekLista <- beolvasFilmek
  let rendezok = nub (map rendezo filmekLista)
      sorok = map (\r ->
        let rendezoFilmjei = filter (\f -> rendezo f == r) filmekLista
            db = length rendezoFilmjei
            atlagErtekeles = sum (map ertekeles rendezoFilmjei) / fromIntegral db
        in r ++ ", " ++ show db ++ ", " ++ show atlagErtekeles
        ) rendezok
  writeFile "12.labor/rendezok.txt" (unlines sorok)
  putStrLn "\nrendezok.txt sikeresen elmentve!"