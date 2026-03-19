import Graphics.Win32 (peekSIZE, iCON_BIG)
import Distribution.PackageDescription (maybeToLibraryName)
import Control.Monad (when)
-- # 4. labor

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- - az első n páros szám négyzetét,
parosNegyzet n = [i ^ 2 | i <- [0,2..n * 2]]

parosNegyzet2 n =take n [i ^ 2 | i <- [0,2..n * 2]]

-- - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,

szamokLs n

    | n == 1 = replicate n n
    | otherwise = szamokLs (n-1)++replicate n n

szamokLs2 n i
    | i /=n = replicate i i ++ szamokLs2 n (i+1)
    |otherwise = replicate i i
-- - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,

szamokLs3 n i
    | i /=n = replicate i (i*2) ++ szamokLs3 n (i+1)
    |otherwise = replicate i (i*2)

-- - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$,

lsN n = [n,n-1..1]++[1..n]

lsN2 n = reverse [1..n]++[1..n]
-- - váltakozva tartalmazzon True és False értékeket,

tf n = [even i | i <- [0..n]]

tf2 n =take n ls
    where
        ls = [True,False]++ ls

tf3 n = concat $ replicate n [True,False]

-- - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.

nullEgyMinEgy n =take n ls
    where
        ls = [0,1,-1]++ ls

nullEgyMinEgy2 n = concat $ replicate n [0,1,-1]

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy adott szám osztóinak számát,

osztok n = [i | i <- [1..n],mod n i == 0]

osztokSz n = length ( osztok n)
osztokSz2 n = myLength $ osztok n
    where
        myLength [] = 0
        myLength (_:ls)=1+myLength ls

osztokSz3 n = foldl (\res i -> if mod n i == 0 then res+1 else res) 0 [1..n]
-- - meghatározza egy adott szám legnagyobb páratlan osztóját,

lnP n = maximum [i | i <- osztok n,odd i]

lnP2 n = last [i | i <- [1,3..n],mod n i == 0]

lnP3 n = foldl (\res i -> if n `mod` i == 0 then i else res) 1 [1,3 .. n]
-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,

decP x p
    | x < 0 = error "neg.szam"
    | x<p = [x]
    |otherwise = decP (div x p) p ++ [mod x p]

decPSzam x p =length $ decP x p

decPSzam2 x p = myLength $ decP x p
    where
        myLength [] = 0
        myLength (_:ls)=1+myLength ls

myLength2 [] = 0
myLength2 (_:ls)=1+myLength2 ls


decPSzam3 x p =myLength2 $ decP x p
-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,

decPMax x p = maximum $ decP x p
-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.

fibo = fiboSg 0 1 0
    where
        fiboSg a b res = res : fiboSg b res (res+b)

fiboAB a b = (dropWhile (<a) . takeWhile (<b)) fibo

fiboAB2 a b = dropWhile (<a) $ takeWhile (<b) fibo

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,

atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

pozAtlag ls = atlag [i | i <- ls,i>0]

pozAtlag2 ls = atlag $ filter (>0) ls

-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,

listaNElem ls n = [i | (idx ,i) <- zip [1..] ls ,mod idx n ==0]

listaNElem2 ls n i
    |i>=length ls =[]
    | mod i n == 0 = ls !! i : listaNElem2 ls n (i+1)
    |otherwise = listaNElem2 ls n (i+1)


-- - tükrözi egy lista elemeit,


tukor ls = reverse ls

tukor2 ls = foldl (\res x ->  x:res)[] ls

--A szamokat tukrozi a listaban: 
tukor3 ls = map (\x -> seged x 0) ls
    where
        seged 0 acc = acc
        seged x acc = seged (div x 10) (acc * 10 + mod x 10)


-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
-- - meghatározza egy lista leggyakrabban előforduló elemét.
