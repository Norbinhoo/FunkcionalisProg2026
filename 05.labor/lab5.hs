import Data.Char
-- # 5. labor

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.

-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,
-- - meghatározza egy lista pozitív elemeinek összegét,
-- - egy lista páros elemeinek szorzatát,
-- - n-ig a négyzetszámokat.
-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$

-- III.

-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120,"ms"),("fecske",85,"ms"),("cinege",132,"ms")]
--   > osszegT ls
--   337
--   ```
-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```



tokenize :: [Char] -> [String]
tokenize = words . map (irasjelHelyettesit . toLower)
--tokenize =  map (irasjelHelyettesit . toLower)

irasjelHelyettesit :: Char -> Char
irasjelHelyettesit c
    | notElem c ",.;:!?\"'()[]<>" = c
    | otherwise = ' '

lengthLista ls = map length ls

myMinimum2 :: (Num b, Enum b, Ord a) => [a] -> (a, [b])
myMinimum2 ls = (m, map snd $ filter fg $ zip ls [0,1..])
    where
        m = minimum ls
        fg k = fst k == m

--myMinimum :: (Num b, Enum b, Ord a) => [a] -> (a, [b])
myMinimum ls = (m, map snd $ filter fg $ zip ls [0,1..])
    where
        m = minimum ls
        fg k = fst k == m



talalat x ls= l1
    where 
        zipls = zip ls [0..]
        l1 = map snd $ filter (\y -> fst y == x) zipls


ps ls = sum [t2 | (t1,t2,t3) <- ls]
ps2 ls r = sum [t2 | (t1,t2,t3) <- ls,t3==r]
    where ls1 = filter (\(t1,t2,t3)->t3 == r) ls1
main = do
    let lista = " egy PrOBA szoveg. ezz azz egy masik proBa! azz Tobbfele irasJEL ::Hasznalat"
    let l1 = tokenize lista
    let l2 = lengthLista l1 
    --let m1 = minimum l2
    putStrLn "a szavak hossza: "
    print l2
    let (minim,indexek) = myMinimum2 l2
    let l3 =  zip l1 l2
    print l3
    let r1 = concatMap (++ " ") [fst (l3 !! i) | i <- indexek]
    print r1


    let a = 5
    let l1 =[3, 13, 5, 6, 7, 12, 5, 8, 5]
    let t1 = talalat a l1
    let a2='e'
    let l2="Bigeri-vizeses"
    let t2 = talalat a2 l2
    --print t1
    let c1 = concatMap ((<> " ") . show) t1
    putStrLn $ show a <> " talalat pozicioi" <> c1
    let c2 = concatMap ((<> " ") . show) t2
    putStrLn $ show a2 <> " talalat pozicioi" <> c2
    --print t2
    let c3 = concatMap ((++ " "). show) t1
    print c3


    let ls = [("golya",120,"ms"),("fecske",85,"cv"),("cinege",132,"ms")]
    let eredmeny = ps ls
    let madarls =concatMap  (<> " ") [t1 | (t1,t2,t3) <- ls]
    putStrLn $ madarls <> "populaciok szma: " ++ show eredmeny 
    
    let ls1 = filter (\(t1,t2,t3)-> t3 == "ms") ls
    let madarls2 = concatMap  (<> " ") [t1 | (t1,t2,t3) <- ls1]
    let eredmeny2 = ps2 ls "ms" 
    putStrLn $ madarls2 <> " " <> "ms" <> "-ben populacio szam: " ++ show eredmeny2


        
