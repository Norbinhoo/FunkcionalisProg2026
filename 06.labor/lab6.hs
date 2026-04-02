import GHC.Base (VecElem(Int16ElemRep))
import System.Win32 (lOCALE_S2359)
import Data.List (intercalate)
-- III lab5

atlagTu ls = [(nev,atlag jegyek) | (nev , jegyek )<- ls]
    where 
        atlag ls2 = sum ls2/fromIntegral (length ls2)


atlagTu2 ls = mapM_ (\(nev,atlagJegyek) -> putStrLn (nev ++ " " ++  show atlagJegyek)) ls2
    where 
        ls2=[(nev,atlag jegyek) | (nev,jegyek) <-ls]
        atlag ls2 = sum ls2/fromIntegral (length ls2)

-- # 6. labor

-- I. Írjunk egy-egy Haskell függvényt, amely beolvass a billentyűzetről két természetes számot és kiírja a képernyőre

main1 = do
    putStr "x1="
    x1<-readLn :: IO Int
    putStr "x2="
    x2<-readLn :: IO Int
    print("x1=" ++ show x1 ++ ",x2= " ++ show x2)
    let ls = if x1<x2 then [x1..x2] else [x2..x1]
        voLs=legtobbVO ls
    putStrLn "Lista elemei: "
    print ls 
    putStrLn ("Lista elemeinek osszege: " ++ show (sum ls))
    putStrLn ("Primszamok osszege: " ++ show (primSzamOsszeg ls))
    putStrLn ("Legtobb valodi oszto szam: " ++ show (snd .head $ voLs) ++" ezzel rendelkezo szamok:")
    print (map fst voLs)


main1_2 = do
    putStr "x1="
    x1<-getLine
    putStr "x2="
    x2<-getLine
    let x1Szam = read x1 :: Int
        x2Szam = read x2 :: Int
    putStrLn("x1=" ++ show (read x1 :: Int) ++ " x2=" ++ show x2)
    putStrLn("x1=" ++ show x1Szam ++ " x2=" ++ show x2Szam)



-- - a két szám közötti számok összegét,
-- - a két szám közötti prímszámok összegét,

osztok x = [i | i <- [1..x],mod x i == 0]

primszam x = [1,x] == osztok x

primSzamOsszeg ls = sum . filter primszam $ ls
-- - a két szám közötti azon számokat, amelyeknek legtöbb valódi osztója van.

valodiOsztok x = [i | i <- [2..div x 2],mod x i == 0,i /=x]

legtobbVO ls = filter(\(szam,vo) -> vo == maxVO) ls2 
    where 
        ls2=[(szam,length $ valodiOsztok szam) | szam <-ls]
        maxVO = maximum $ map snd ls2

-- II. Írjunk egy-egy Haskell függvényt, amely beolvassa a billentyűzetről az n természetes számot és kiírja a képernyőre


main2 = do
    putStr "n="
    n<-readLn :: IO Int
    putStr ("n=" ++ show n)
    let fiboLs=fibo n
    putStrLn "mapM_-el"
    mapM_ (\x ->putStr (show x ++ " "))fiboLs
    putStrLn "\nunwords-el"
    putStrLn $ unwords $ map show fiboLs
    putStrLn "intercalate-tel"
    putStrLn $ intercalate " " $ map show fiboLs



-- - n-ig a Fibonacci számok listáját ($n > 50$), úgy hogy a számok közé szóközt ír,

fibo n = dropWhile(<50) $ takeWhile (<n) $ fiboSg 0 1 0 
   where
    fiboSg a b res = res : fiboSg b res (res + b)

--meg le kell kezelni hogy >50
fibo2 n a b res
    | res < n = res : fibo2 n b res (res+b)
    |otherwise = [res]
-- - n-ig a prímszámok listáját, úgy hogy a számok közé szóközt ír,
-- - az n kettes számrendszerbeli alakját, úgy hogy minden negyedik bit után egy szóközt ír,
-- - az n 16-os számrendszerbeli alakját, úgy hogy minden két szimbólum után egy szóközt ír, illetve az a, b, c, d, e, f szimbólumokat használja a 10-nél nagyobb számjegyek kódolására,
-- - az n értékének megfelelően a következő sorokat:

--   ```
--   (0, 0)
--   (0, 1) (1, 0)
--   (0, 2) (1, 1) (2, 0)
--   ...
--   (0, n) (1, n-1), ..., (n, 0)
--   ```
-- - az n értékének megfelelően az összes természetes szám kettes számrendszerbeli alakját,
--   például: $$n = 6:\ 0,\ 1,\ 10,\ 11,\ 100,\ 101,\ 110$$.

-- III. Írjunk egy-egy Haskell függvényt, amely a billentyűzetről olvas be egész számokat egy listába, majd kiírja a képernyőre, hogy

-- - hányszor szerepel egy adott egész szám a listában,
-- - melyek azok az egész számok, amelyek kisebbek a listában megadott számok átlagértékénél,
-- - minden listabeli elem hányszor szerepel a listában, azaz készítsünk elem előfordulási statisztikát.

-- IV. Írjunk egy-egy Haskell függvényt, amely a billentyűzetről végjelig olvas be karakterláncokat, és

-- - meghatározza a legnagyobbat,
-- - meghatározza a legnagyobb elemek indexét,
-- - az adatok rendezett sorrendjét.

main3 = do
    let lsNevJegy=[("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
    mapM_ (\(nev,atlagJegyek) -> putStrLn (nev ++ " " ++  show atlagJegyek)) (atlagTu lsNevJegy)
    putStrLn "\nCsak meghivas\n"
    atlagTu2 lsNevJegy