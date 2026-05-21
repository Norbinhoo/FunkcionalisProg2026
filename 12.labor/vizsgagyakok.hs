import Data.List (maximumBy, minimumBy, sortBy)
import Data.List.Split (splitOn) -- Beépített split csomag használata
import Data.Ord (Down (..), comparing)
import System.IO

-- A kért adatszerkezet
data Diak = Diak
  { nev :: String,
    szak :: String,
    evfolyam :: Int,
    atlag :: Double,
    hitelpont :: Int
  }
  deriving (Show, Eq)

-- CSV sor feldolgozása Diak struktúrává
parseLine :: String -> Diak
parseLine line =
  let reszek = splitOn "," line
   in Diak
        { nev = reszek !! 0,
          szak = reszek !! 1,
          evfolyam = read (reszek !! 2) :: Int,
          atlag = read (reszek !! 3) :: Double,
          hitelpont = read (reszek !! 4) :: Int
        }

-- Diak visszaalakítása CSV sorrá
toCSVLine :: Diak -> String
toCSVLine d = nev d ++ "," ++ szak d ++ "," ++ show (evfolyam d) ++ "," ++ show (atlag d) ++ "," ++ show (hitelpont d)

-- Segédfüggvény a szakok egyedi kigyűjtéséhez
kigyujtSzakok :: [Diak] -> [String]
kigyujtSzakok [] = []
kigyujtSzakok (d : ds) = szak d : kigyujtSzakok (filter (\x -> szak x /= szak d) ds)

-- GOLYÓÁLLÓ BEOLVASÁS: Kiszűri a hibás/hiányos/üres sorokat, így nincs "index too large" hiba!
beolvasDiakok :: IO (String, [Diak])
beolvasDiakok = do
  tartalom <- readFile "12.labor/diakok.csv"
  let mindenSor = filter (not . null) (lines tartalom)
  if null mindenSor
    then return ("nev,szak,evfolyam,atlag,kreditszam", [])
    else do
      let fejlec = head mindenSor
          adatSorok = tail mindenSor
          -- Csak azt a sort dolgozzuk fel, ami a darabolás után pont 5 elemű (biztonsági szűrő)
          szurtAdatSorok = filter (\sor -> length (splitOn "," sor) == 5) adatSorok
      return (fejlec, map parseLine szurtAdatSorok)

-- ============================================================================
-- FELADATOK FÜGGVÉNYEI
-- ============================================================================

-- a. feladat: Szak szűrése, átlaga és legjobbja
feladatA :: IO ()
feladatA = do
  (_, diakok) <- beolvasDiakok
  putStr "Kerem adjon meg egy szakot: " >> hFlush stdout
  szurtSzak <- getLine
  let szurt = filter (\d -> szak d == szurtSzak) diakok
  
  if null szurt
    then putStrLn "Nincs ilyen szak az adatbázisban!"
    else do
      putStrLn $ "\nAz " ++ szurtSzak ++ " szakhoz tartozo diakok:"
      mapM_ (\d -> putStrLn $ nev d ++ " - " ++ show (evfolyam d) ++ " - " ++ show (atlag d) ++ " - " ++ show (hitelpont d)) szurt
      
      let szakAtlag = sum (map atlag szurt) / fromIntegral (length szurt)
      putStrLn $ "Az " ++ szurtSzak ++ " szak diakjainak atlaga " ++ show szakAtlag
      
      let legjobb = maximumBy (comparing atlag) szurt
      putStrLn $ "Az " ++ szurtSzak ++ " szak legjobb atlaggal rendelkezo diakja\n\n" ++ nev legjobb ++ ", atlaga " ++ show (atlag legjobb) ++ "."

-- b. feladat: Évfolyamonkénti statisztika
feladatB :: IO ()
feladatB = do
  (_, diakok) <- beolvasDiakok
  mapM_ (\ev -> do
    let evDiakjai = filter (\d -> evfolyam d == ev) diakok
        db = length evDiakjai
    if db > 0 
      then putStrLn $ show ev ++ ". evfolyam: " ++ show db ++ " diak, atlag: " ++ show (sum (map atlag evDiakjai) / fromIntegral db)
      else putStrLn $ show ev ++ ". evfolyam: 0 diak, atlag: 0.0"
    ) [1..4]

-- c. feladat: Kitűnők mentése (átlag > 9.0 és kredit >= 30)
feladatC :: IO ()
feladatC = do
  (_, diakok) <- beolvasDiakok
  let kitunok = filter (\d -> atlag d > 9.0 && hitelpont d >= 30) diakok
  writeFile "12.labor/kituno.txt" (unlines $ map toCSVLine kitunok)
  putStrLn "kituno.txt elmentve."

-- d. feladat: Rendezett mentése (átlag csökkenő, majd név szerint)
feladatD :: IO ()
feladatD = do
  (fejlec, diakok) <- beolvasDiakok
  let rendezett = sortBy (\d1 d2 -> case comparing (Down . atlag) d1 d2 of { EQ -> comparing nev d1 d2; res -> res }) diakok
  writeFile "12.labor/rendezett.csv" (unlines (fejlec : map toCSVLine rendezett))
  putStrLn "rendezett.csv elmentve."

-- e. feladat: Legjobb és legrosszabb átlag
feladatE :: IO ()
feladatE = do
  (_, diakok) <- beolvasDiakok
  let rossz = minimumBy (comparing atlag) diakok
      jo = maximumBy (comparing atlag) diakok
  putStrLn $ "Legrosszabb atlagu diak: " ++ nev rossz ++ " (" ++ show (atlag rossz) ++ ")"
  putStrLn $ "Legjobb atlagu diak: " ++ nev jo ++ " (" ++ show (atlag jo) ++ ")"

-- f. feladat: Legkevesebb és legtöbb kredit
feladatF :: IO ()
feladatF = do
  (_, diakok) <- beolvasDiakok
  let keves = minimumBy (comparing hitelpont) diakok
      sok = maximumBy (comparing hitelpont) diakok
  putStrLn $ "Legkevesebb kredittel rendelkezo diak: " ++ nev keves ++ " (" ++ show (hitelpont keves) ++ ")"
  putStrLn $ "Legtobb kredittel rendelkezo diak: " ++ nev sok ++ " (" ++ show (hitelpont sok) ++ ")"

-- g. feladat: Ösztöndíj szűrés bekért átlag alapján
feladatG :: IO ()
feladatG = do
  (fejlec, diakok) <- beolvasDiakok
  putStr "Kerem adjon meg egy minimalis atlagot: " >> hFlush stdout
  limitStr <- getLine
  let limit = read limitStr :: Double
      szurt = filter (\d -> atlag d >= limit) diakok
  writeFile "12.labor/osztondij.csv" (unlines (fejlec : map toCSVLine szurt))
  putStrLn "osztondij.csv elmentve."

-- h. feladat: Melyik szakon van a legtöbb diák
feladatH :: IO ()
feladatH = do
  (_, diakok) <- beolvasDiakok
  let szakok = kigyujtSzakok diakok
      szamlalo = map (\sz -> (sz, length (filter (\d -> szak d == sz) diakok))) szakok
      legnepesebb = maximumBy (comparing snd) szamlalo
  putStrLn $ "A legnepesebb szak: " ++ fst legnepesebb ++ " (" ++ show (snd legnepesebb) ++ " diak)"

-- i. feladat: Melyik szakon a legmagasabb az átlag
feladatI :: IO ()
feladatI = do
  (_, diakok) <- beolvasDiakok
  let szakok = kigyujtSzakok diakok
      atlagok = map (\sz -> let szDiak = filter (\d -> szak d == sz) diakok in (sz, sum (map atlag szDiak) / fromIntegral (length szDiak))) szakok
      legjobb = maximumBy (comparing snd) atlagok
  putStrLn $ "A legmagasabb atlagu szak: " ++ fst legjobb ++ " (atlag: " ++ show (snd legjobb) ++ ")"

-- j. feladat: Új diák hozzáadása (Biztonságos, soronkénti beolvasással)
feladatJ :: IO ()
feladatJ = do
  putStrLn "Uj diak adatainak bevitele:"
  
  putStr "Nev: "
  hFlush stdout
  uNev <- getLine
  
  putStr "Szak: "
  hFlush stdout
  uSzak <- getLine
  
  putStr "Evfolyam: "
  hFlush stdout
  uEvStr <- getLine
  
  putStr "Atlag: "
  hFlush stdout
  uAtlStr <- getLine
  
  putStr "Kreditszam: "
  hFlush stdout
  uKredStr <- getLine
  
  let ujSor = uNev ++ "," ++ uSzak ++ "," ++ uEvStr ++ "," ++ uAtlStr ++ "," ++ uKredStr
  appendFile "diakok.csv" ("\n" ++ ujSor)
  putStrLn "Az uj diak hozzaadva a diakok.csv-hez!"

-- Főprogram
main :: IO ()
main = putStrLn "Inditsd el a feladatokat kulon: feladatA, feladatB, feladatC, stb."