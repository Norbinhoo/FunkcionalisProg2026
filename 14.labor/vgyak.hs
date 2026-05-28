import System.IO
import Data.List (sortBy, intercalate, group, sort)
import Data.Function (on)
import Text.Printf (printf)

-------------------------------------------------------------------------
-- KÖZÖS ADATSZERKEZET ÉS BEOLVASÓ FÜGGVÉNYEK
-------------------------------------------------------------------------

data Allat = Allat {
    nev             :: String,
    kontinens       :: String,
    fajcsoport      :: String,
    veszelyeztetett :: Bool,
    eletkor         :: Int
} deriving (Show, Eq)

-- A kért wordsBy függvény a javított scope-pal
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s =
    case dropWhile p s of
        "" -> []
        s' -> let (w, s'') = break p s'
              in w : wordsBy p s''

createAllat :: [String] -> Allat
createAllat sorLista = Allat {
    nev             = sorLista !! 0,
    kontinens       = sorLista !! 1,
    fajcsoport      = sorLista !! 2,
    veszelyeztetett = (sorLista !! 3) == "True",
    eletkor         = read (sorLista !! 4) :: Int
}

allatToRow :: Allat -> String
allatToRow a = printf "%s %s %s %s %d" (nev a) (kontinens a) (fajcsoport a) (show (veszelyeztetett a)) (eletkor a)

toLowerStr :: String -> String
toLowerStr = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

-- Segédfüggvény, hogy ne kelljen minden feladatban újraírni a beolvasást
beolvasAllatok :: IO [Allat]
beolvasAllatok = do
    content <- readFile "14.labor/allatok.txt"
    let rows = map (wordsBy (== ' ')) (lines content)
    return (map createAllat (filter (\r -> length r >= 5) rows))

-------------------------------------------------------------------------
-- FELADATOK (Külön do blokkokban)
-------------------------------------------------------------------------

-- a. feladat: Életkor szerinti rendezés és kiírás
felA = do
    allatok <- beolvasAllatok
    let rendezettEletkor = sortBy (compare `on` eletkor) allatok
    writeFile "14.labor/rendezett_allatok.txt" (unlines (map allatToRow rendezettEletkor))
    putStrLn "a. feladat kész: 'rendezett_allatok.txt' létrehozva."

-- b. feladat: Kontinens statisztika billentyűzetről
felB = do
    allatok <- beolvasAllatok
    putStr "Kerlek, adj meg egy kontinenst (pl. Afrika): "
    hFlush stdout
    keresettKontinens <- getLine
    
    let kontinensSzurt = filter (\a -> toLowerStr (kontinens a) == toLowerStr keresettKontinens) allatok
    
    let (atlageletkor, allatokVesszovel, szam, veszelyeztetettSzam) = 
            if null kontinensSzurt
            then (0.0 :: Double, "Nincs adat", 0, 0)
            else (
                (fromIntegral (sum (map eletkor kontinensSzurt)) / fromIntegral (length kontinensSzurt)) :: Double,
                intercalate ", " (map nev kontinensSzurt),
                length kontinensSzurt,
                length (filter veszelyeztetett kontinensSzurt)
            )
            
    let kimenetKontinens = printf "A `%s` adatait:\n\n- atlageletkor: `%.1f`\n- a kontinensen elo allatok: `%s`\n- a kontinensen elo allatok szama: `%d`\n- a kontinensen elo veszelyeztett allatok szama: `%d`\n" 
                            keresettKontinens atlageletkor allatokVesszovel szam veszelyeztetettSzam
    writeFile "14.labor/kontinens_allatok.txt" kimenetKontinens
    putStrLn "b. feladat kész: 'kontinens_allatok.txt' elmentve."

-- c. feladat: Fajcsoportok megoszlása százalékban
felC = do
    allatok <- beolvasAllatok
    let osszesen = length allatok
    putStrLn "c. feladat (Fajcsoportok megoszlasa):"
    let csoportok = group (sort (map fajcsoport allatok))
    mapM_ (\cs -> do
            let csoportNev = head cs
            let db = length cs
            let szazalek = (fromIntegral db / fromIntegral osszesen) * 100 :: Double
            printf "   - %s: %d db (%.1f%%)\n" csoportNev db szazalek
          ) csoportok

-- d. feladat: Veszélyeztetettségi statisztika százalékban
felD = do
    allatok <- beolvasAllatok
    let osszesen = length allatok
    putStrLn "d. feladat (Veszelyeztettsegi statisztika):"
    let vDb = length (filter veszelyeztetett allatok)
    let nvDb = osszesen - vDb
    let vSzazalek = (fromIntegral vDb / fromIntegral osszesen) * 100 :: Double
    let nvSzazalek = (fromIntegral nvDb / fromIntegral osszesen) * 100 :: Double
    printf "   - Veszelyeztetett fajok: %.1f%%\n" vSzazalek
    printf "   - Nem veszelyeztetett fajok: %.1f%%\n" nvSzazalek

-- e. feladat: 10 évnél idősebbek névsorban
felE = do
    allatok <- beolvasAllatok
    let idosek = filter (\a -> eletkor a > 10) allatok
    let idosekRendezett = sortBy (compare `on` nev) idosek
    writeFile "14.labor/idosek.txt" (unlines (map allatToRow idosekRendezett))
    putStrLn "e. feladat kész: 'idosek.txt' elmentve."