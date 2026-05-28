import System.IO
import Data.List (sortBy, intercalate, group, sort, groupBy, minimumBy, maximumBy)
import Data.Function (on)
import Text.Printf (printf)

-------------------------------------------------------------------------
-- KÖZÖS ADATSZERKEZET ÉS BEOLVASÓ FÜGGVÉNYEK
-------------------------------------------------------------------------

data Elelmiszer = Elelmiszer {
    nev        :: String,
    kategoria  :: String,
    ar         :: Double,
    mennyiseg  :: Int,
    lejart     :: Bool
} deriving (Show, Eq)

-- A mintád szerinti daraboló függvény
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s =
    case dropWhile p s of
        "" -> []
        s' -> let (w, s'') = break p s'
              in w : wordsBy p s''

createElelmiszer :: [String] -> Elelmiszer
createElelmiszer sorLista = Elelmiszer {
    nev        = sorLista !! 0,
    kategoria  = sorLista !! 1,
    ar         = read (sorLista !! 2) :: Double,
    mennyiseg  = read (sorLista !! 3) :: Int,
    lejart     = (sorLista !! 4) == "True"
}

elelmiszerToRow :: Elelmiszer -> String
elelmiszerToRow e = printf "%s;%s;%.1f;%d;%s" (nev e) (kategoria e) (ar e) (mennyiseg e) (show (lejart e))

toLowerStr :: String -> String
toLowerStr = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

-- Alap fájlbeolvasás a fix 14.labor/ útvonallal
beolvasElelmiszerek :: IO [Elelmiszer]
beolvasElelmiszerek = do
    content <- readFile "14.labor/elelmiszerek.txt"
    let rows = map (wordsBy (== ';')) (lines content)
    return (map createElelmiszer (filter (\r -> length r >= 5) rows))

csoportositKategoria = groupBy ((==) `on` kategoria) . sortBy (compare `on` kategoria)

-------------------------------------------------------------------------
-- FELADATOK (felA, felB, ... DO BLOKKOK)
-------------------------------------------------------------------------

-- a. feladat: Kiíratás kategóriánként ár szerint csökkenő sorrendben
felA = do
    termekek <- beolvasElelmiszerek
    let rendezett = sortBy (\x y -> case compare (kategoria x) (kategoria y) of
                                        EQ -> compare (ar y) (ar x)
                                        ord -> ord) termekek
    putStrLn "a. feladat: Elelmiszerek kategoria es ar szerint csokkenoben:"
    mapM_ (putStrLn . elelmiszerToRow) rendezett


-- b. feladat: Billentyűzetről beolvasott kategória statisztikái
felB = do
    termekek <- beolvasElelmiszerek
    putStr "Kerlek, adj meg egy kategoriat (pl. Gyumolcs, Tejtermek): "
    hFlush stdout
    bekertKat <- getLine
    
    let katSzurt = filter (\e -> toLowerStr (kategoria e) == toLowerStr bekertKat) termekek
    
    if null katSzurt 
        then putStrLn "Nincs ilyen kategoria az adatok kozott!"
        else do
            let osszertek = sum [ar e * fromIntegral (mennyiseg e) | e <- katSzurt]
            let lejartak = filter lejart katSzurt
            let lejartDb = length lejartak
            let lejartNevek = intercalate ", " (map nev lejartak)
            let nemLejartak = filter (not . lejart) katSzurt
            let veszteseg = sum [ar e * fromIntegral (mennyiseg e) | e <- lejartak]
            
            printf "\nA(z) '%s' kategoria adatai:\n" bekertKat
            printf " i. Termekek osszerteke: %.2f Ft\n" osszertek
            printf " ii. Lejart termekfajtak szama: %d db\n" lejartDb
            printf " iii. Lejart termekek listaja: %s\n" (if null lejartNevek then "Nincs" else lejartNevek)
            
            if null nemLejartak
                then putStrLn " iv. Nincs nem lejart termek az arszamitashoz."
                else do
                    let legolcsobb = minimumBy (compare `on` ar) nemLejartak
                    let legdragabb = maximumBy (compare `on` ar) nemLejartak
                    printf " iv. Legolcsobb: %s (%.1f Ft), Legdragabb: %s (%.1f Ft)\n" (nev legolcsobb) (ar legolcsobb) (nev legdragabb) (ar legdragabb)
                    
            printf "  v. Lejart termekekbol szarmazo veszteseg: %.2f Ft\n" veszteseg


-- c. feladat: Legnagyobb és legkisebb veszteségű kategória
felC = do
    termekek <- beolvasElelmiszerek
    let katCsoportok = csoportositKategoria termekek
    let vesztesegek = map (\cs -> (kategoria (head cs), sum [ar e * fromIntegral (mennyiseg e) | e <- cs, lejart e])) katCsoportok
    
    let legmagasabb = maximumBy (compare `on` snd) vesztesegek
    let legalacsonyabb = minimumBy (compare `on` snd) vesztesegek
    
    printf "c. feladat:\n"
    printf "   - Legnagyobb vesztesegu kategoria: %s (%.1f Ft)\n" (fst legmagasabb) (snd legmagasabb)
    printf "   - Legkisebb vesztesegu kategoria: %s (%.1f Ft)\n" (fst legalacsonyabb) (snd legalacsonyabb)


-- d. feladat: Kategóriánként a legkevesebb készletű termék
felD = do
    termekek <- beolvasElelmiszerek
    let katCsoportok = csoportositKategoria termekek
    putStrLn "d. feladat: Kategoriankent a legkevesebb keszletu termekek:"
    mapM_ (\cs -> do
            let legkevesebb = minimumBy (compare `on` mennyiseg) cs
            printf "   - %s kategoria: %s (%d db)\n" (kategoria legkevesebb) (nev legkevesebb) (mennyiseg legkevesebb)
          ) katCsoportok


-- e. feladat: Készletfeltöltési kedvezmények és végösszeg
-- JAVÍTVA: A lokális függvény kisbetűs (szamolas), így megszűnt a mintaillesztési hiba
-- e. feladat: Készletfeltöltési kedvezmények és végösszeg
-- e. feladat: Készletfeltöltési kedvezmények és végösszeg
felE = do
    termekek <- beolvasElelmiszerek
    putStrLn "e. feladat: Termekenked kapott arengedmenyek:"
    
    let szamolas :: [Elelmiszer] -> Double -> IO Double
        szamolas [] akku = return akku
        szamolas (e:es) akku = do
            let eredetiAr = ar e * fromIntegral (mennyiseg e)
            let kedvezmeny = if mennyiseg e >= 70 then 0.15 * eredetiAr else 0.0
            let vegar = eredetiAr - kedvezmeny
            printf "   - %s: eredeti: %.1f, kedvezmeny: %.1f, vegar: %.1f\n" (nev e) eredetiAr kedvezmeny vegar
            szamolas es (akku + vegar)
            
    vegosszeg <- szamolas termekek 0.0
    printf " -> Mindosszesen fizetendo vegosszeg: %.2f Ft\n" (vegosszeg :: Double)


-- f. feladat: 5 új termék beolvasása billentyűzetről és mentése az elelmiszerek_uj.txt-be
felF = do
    termekek <- beolvasElelmiszerek
    putStrLn "f. feladat: Kerlek adj meg 5 uj termeket a kovetkezo formaban: Nev;Kategoria;Ar;Mennyiseg;Lejart-e"
    
    let beolvasUjak 0 = return []
        beolvasUjak n = do
            putStr $ show (6 - n) ++ ". uj termek: "
            hFlush stdout
            sor <- getLine
            let darabolt = wordsBy (== ';') sor
            if length darabolt < 5
                then do
                    putStrLn "Hibas formatum, probald ujra!"
                    beolvasUjak n
                else do
                    let uj = createElelmiszer darabolt
                    maradek <- beolvasUjak (n-1)
                    return (uj : maradek)
                    
    ujTermekek <- beolvasUjak 5
    let osszesTermek = termekek ++ ujTermekek
    
    writeFile "14.labor/elelmiszerek_uj.txt" (unlines (map elelmiszerToRow osszesTermek))
    putStrLn " -> Minden termek sikeresen elmentve ide: 14.labor/elelmiszerek_uj.txt"


-- g. feladat: Lejárt élelmiszerek névsorba rendezve a lejart.txt-be
felG = do
    ujTartalom <- readFile "14.labor/elelmiszerek_uj.txt"
    let rows = map (wordsBy (== ';')) (lines ujTartalom)
    let mindegyikTermek = map createElelmiszer (filter (\r -> length r >= 5) rows)
    
    let lejartak = filter lejart mindegyikTermek
    let rendezettLejartak = sortBy (compare `on` nev) lejartak
    
    writeFile "14.labor/lejart.txt" (unlines (map elelmiszerToRow rendezettLejartak))
    putStrLn "g. feladat kesz: A lejart termekek nevsor szerint elmentve ide: 14.labor/lejart.txt"