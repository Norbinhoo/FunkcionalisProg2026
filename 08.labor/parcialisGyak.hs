import Data.List
import Data.Char (isDigit)

-- 1. Egy [(String, Int)] típusú lista eleme egy városnevet és a megfelelő népesség
-- értéket tárolja. Írjunk egy Haskell függvényt, amely meghatározza, azokat a
-- városokat, amelyek népesség értéke egy adott n értéknél nagyobb. A kapott
-- városneveket ábécé sorrendbe rendezve külön sorba írjuk ki a képernyőre.
-- Például:
-- ● Bemenet: 150000 [("sepsiszentgyorgy", 54000), ("kolozsvár", 330000),
-- ("marosvasarhely", 130000), “temesvar", 310000), ("arad", 160000),
-- ("gyergyoszentmiklos", 18000), ("nagyvarad",196000)]
-- ● Kimenet:
-- A(z) 150000 nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:
-- - arad
-- - kolozsvár
-- - nagyvarad
-- - temesvar
-- ● Amennyiben nincs olyan város, amelyiknek a népesség értéke egy adott n
-- értéknél nagyobb, a következő a kimenet: “Nincs x erteknel nagyobb nepesseg
-- ertekkel rendelkezo varos.”

fel1 n ls = map fst (filter (\(x,y) -> y>n) ls)

fel1Main = do
    let n = 150000
    let ls = [("sepsiszentgyorgy", 54000), ("kolozsvár", 330000), ("marosvasarhely", 130000), ("temesvar", 310000), ("arad", 160000), ("gyergyoszentmiklos", 18000), ("nagyvarad",196000)]
        ls2 = sort $ fel1 n ls
    if ls2==[]
        then putStrLn("Nincs " ++ show n ++ "erteknel nagyobb nepesseg ertekkel rendelkezo varos")
        else do
            putStrLn("A(z) " ++ show n ++ "nepesseg erteknel nagyobbal rendelkezo varaosok a kovetkezok: ")
            mapM_ (\v -> putStrLn (" - " ++ v)) ls2


varosokSzurese n lista = do
    let talalatok = sort [nev | (nev, lakos) <- lista, lakos > n]
    if null talalatok
        then putStrLn $ "Nincs " ++ show n ++ " erteknel nagyobb nepesseg ertekkel rendelkezo varos."
        else do
            putStrLn $ "A(z) " ++ show n ++ " nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:"
            mapM_ (\v -> putStrLn $ "- " ++ v) talalatok
-- 2. Írjunk egy Haskell függvényt, amely meghatározza egy bemeneti egész
-- számokat tartalmazó lista azon elemeit, amelyek nem tartalmazzák a 0
-- számjegyet. Az eredmény számokat szóközzel elválasztva írjuk ki a
-- képernyőre.
-- Például:
-- ● Bemenet: [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
-- ● Kimenet: A 0 szamjegyet nem tartalmazo szamok a kovetkezok: 3223 816252
-- 23561 61
-- ● Amennyiben nincsenek ilyen számok, a kimenet a következő: “Nincsenek
-- olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet.”

nincsNulla x = not ('0' `elem` show x)

fel2 = do
    let ls = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
        ls2 =[102,103,104]
        jo = filter nincsNulla ls
    if jo == []
        then putStrLn "Nincsenek olyan szamok ,amelyeknek nem tartalmazzak a 0 szamjegyet"
        else putStrLn ("A 0 szamjegyet nem tartalmazo szamok a kovetkezok: " ++ intercalate " " (map show jo)) --vagy intercalate helyett unwords!!!

nincsBenneNulla lista = do
    let eredmeny = [x | x <- lista, '0' `notElem` show x]
    if null eredmeny
        then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
        else do
            putStr "A 0 szamjegyet nem tartalmazo szamok a kovetkezok: "
            putStrLn $ unwords (map show eredmeny)
-- 3. Egy listában karakterláncok vannak, írjunk egy Haskell programot, amely kiírja
-- azokat a karakterláncokat a képernyőre egymás alá rendezve ábécé
-- sorrendbe, amelyekben nincsenek számjegyek.
-- Például:

-- ● Bemenet: ["2023tuple", "function", "float", "higher-order", "variable10",
-- "may13be", "0recursion", "monad", "class"]
-- ● Kimenet:
-- A karakterlancok, amelyek nem tartalmaznak szamokat:
-- class
-- float
-- function
-- Higher-order
-- monad
-- ● Amennyiben nincsenek ilyen karakterláncok, a kimenet a következő:
-- “Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot.”

nincsSzam x = not (any isDigit x)

fel3 = do 
    let ls = ["2023tuple", "function", "float", "higher-order", "variable10","may13be", "0recursion", "monad", "class"]
        jo = sort $ filter nincsSzam ls 
    if jo == []
        then putStrLn "Nincsenek olyan karakterlancok,amelyek nem tartalmaznak szamot:"
        else do
            putStrLn "A karakterlancok,amelyek nem tartalmaznak szamokat: "
            mapM_ putStrLn jo -- vagy atadaskor is lehet rendezni

szamjegyNelkuli lista = do
    let szurt = sort [s | s <- lista, not (any isDigit s)]
    if null szurt
        then putStrLn "Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot."
        else do
            putStrLn "A karakterlancok, amelyek nem tartalmaznak szamokat:"
            mapM_ putStrLn szurt

-- 4. Írjunk egy Haskell programot, amely meghatározza, hogy az s karakterláncnak
-- melyek a szomszédjai az lsS karakterláncokat tartalmazó listából, ahol egy
-- karakterlánc szomszédjait az ábécé sorrend szerinti kell érteni.
-- Például:
-- ● Bemenet:
-- s = feri
-- lsS = Mari Zsuzsa szidi Lori kata feri teri Dani zsolti
-- ● Kimenet: feri baloldali szomszedja Zsuzsa, jobboldali szomszedja pedig kata

getNeighbors s lsS = 
    let rendezett = sort lsS
    in case elemIndex s rendezett of
        Nothing -> (Nothing, Nothing) -- Ha nincs benne a listában
        Just i  -> ( if i == 0 then Nothing else Just (rendezett !! (i - 1))
                   , if i == length rendezett - 1 then Nothing else Just (rendezett !! (i + 1))
                   )

main :: IO ()
main = do
    let s = "feri"
    let lsS = ["Mari", "Zsuzsa", "szidi", "Lori", "kata", "feri", "teri", "Dani", "zsolti"]
    
    let (bal, jobb) = getNeighbors s lsS
    
    case (bal, jobb) of
        (Nothing, Nothing) -> putStrLn "A megadott szo nem talalhato a listaban."
        _ -> do
            putStr $ s ++ " "
            case bal of
                Just b  -> putStr $ "baloldali szomszedja " ++ b
                Nothing -> putStr "nincs baloldali szomszedja"
            
            putStr ", "
            
            case jobb of
                Just j  -> putStrLn $ "jobboldali szomszedja pedig " ++ j
                Nothing -> putStrLn "nincs jobboldali szomszedja."

szomszedok s nyersLista = do
    let rendezett = sort (words nyersLista)
    case elemIndex s rendezett of
        Nothing -> putStrLn "A keresett elem nincs a listaban."
        Just i  -> do
            let bal = if i > 0 then rendezett !! (i-1) else "nincs"
            let jobb = if i < length rendezett - 1 then rendezett !! (i+1) else "nincs"
            putStrLn $ s ++ " baloldali szomszedja " ++ bal ++ ", jobboldali szomszedja pedig " ++ jobb

-- 5. Egy [(String, Int, Int)] típusú lista eleme egy telefon márkanevet, egy eladási
-- értéket, és egy árat tartalmaz. Írjunk egy Haskell programot, amely
-- meghatározza azokat a telefonokat, amelyekből a legtöbbet adtak el, illetve
-- mennyi volt ez az érték. Az eredmény márkaneveket rendezve egymás alá
-- írjuk, amelyek elé írjuk ki egy kisérő szöveggel együtt a maximális eladási
-- értéket.
-- Például:
-- ● Bemenet: [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25,
-- 3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),
-- ("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
-- ● Kimenet: A maximalis eladasi ertek 30. A telefonok, amelyeknek ennyi az
-- eladasi erteke a kovetkezok:
-- - iphone3
-- - huaweiS1
-- - samsungA1

snd3Elem (_,y,_) = y
fst3Elem (x,_,_) = x
fel5 = do
    let ls = [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25,3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
    let ls2 = reverse $ sortOn snd3Elem ls
        ls3 = sort ls --elso elem szerint rendez
        maxErtek = snd3Elem (head ls2)
        megoldas = takeWhile (\(_,e,_) -> e == maxErtek) ls2
    putStrLn ("A maximalis eladasi ertek " ++ show maxErtek ++ ". A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok:")
    mapM_ (putStrLn . show) megoldas
    print ls2


maxEladas lista = do
    let eladasok = [e | (_, e, _) <- lista]
    if null eladasok 
        then putStrLn "Ures lista."
        else do
            let m = maximum eladasok
            let nevek = sort [nev | (nev, e, _) <- lista, e == m]
            putStrLn $ "A maximalis eladasi ertek " ++ show m ++ ". A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok:"
            mapM_ (\n -> putStrLn $ "- " ++ n) nevek
-- 6. Írj egy Haskell függvényt, melynek egy lista a bemenete, és megadja azokat a
-- számokat, amelyek előfordulási száma páratlan. Az eredményt írasd ki a
-- példában szereplő formában, előfordulási érték szerint rendezve.
-- Például:
-- ● Bemenet: [7]
-- ● Kimenet: Elofordulas: 1 -> Ertek: 7
-- ● Bemenet: [1, 1, 2]
-- ● Kimenet: Elofordulas: 1 -> Ertek: 2
-- ● Bemenet: [1, 1]
-- ● Kimenet: Nincs paratlan elofordulasi ertekkel rendelkezo szam.
-- ● Bemenet: [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]
-- ● Kimenet:
-- Elofordulas: 1 -> Ertek: 3
-- Elofordulas: 1 -> Ertek: 7
-- Elofordulas: 3 -> Ertek: 4
-- Elofordulas: 5 -> Ertek: 2

paratlanElofordulas lista = do
    -- Csoportosítunk: [[1,1], [2,2,2], [3]] -> kiszámoljuk a hosszt és az értéket
    let statisztika = [(length g, head g) | g <- group (sort lista), odd (length g)]
    -- Rendezés előfordulás szerint (első elem a tuple-ben)
    let rendezett = sortOn fst statisztika
    
    if null rendezett
        then putStrLn "Nincs paratlan elofordulasi ertekkel rendelkezo szam."
        else mapM_ (\(db, ertek) -> putStrLn $ "Elofordulas: " ++ show db ++ " -> Ertek: " ++ show ertek) rendezett