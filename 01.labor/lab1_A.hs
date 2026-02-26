import Control.Monad.Trans.Cont (reset)
-- két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg a b = a + b
kulonbseg :: Double->Double->Double
kulonbseg a b = (-) a b
szorzat a b = a * b
hanyados a b = a / b
osztmar a b = div a b
osztmar2 a b = a `mod` b
-- egy első fokú egyenlet gyökét,
elsof a b = (-b)/a
-- egy szám abszulút értékét,
abszolut a = if a < 0 then -a else a
abszolut2 a 
    | a < 0 = -a
    |otherwise = a
-- egy szám előjelét,
elojel a = if a < 0 then "negativ" else if a > 0 then "pozitiv" else "nulla"

elojel2 a 
    | a < 0 = "negativ"
    | a > 0 = "pozitiv"
    | otherwise = "nulla"
-- két argumentuma közül a maximumot,

max1 a b = if a > b then a else b
max2 :: Ord a => a -> a -> a
max2 a b
    | a > b = a
    | otherwise = b
-- két argumentuma közül a minimumot,
min1 a b = if a < b then a else b

min2 a b
    | a < b = a
    | otherwise = b
-- egy másodfokú egyenlet gyökeit,
masodF a b c 
    | delta < 0 = error "komplex szamok"
    |otherwise = (gy1,gy2)
    where
        delta = b**2 - 4*a*c
        gy1 = (-b + sqrt delta) / (2*a)
        gy2 = (-b - sqrt delta) / (2*a)
-- hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--  Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
elempar ep1 ep2 = if ( a == c && b == d ) || (a == d && b == c) then True else False
    where 
        (a,b)=ep1
        (c,d)=ep2

elempar2 (a,b) (c,d) = (a==c && b==d) || (a==d && b==c)

-- az n szám faktoriálisát (3 módszer),
fakt1 0 = 1
fakt1 n=n*fakt1 (n-1)

fakt2 n
    |n<0=error "neg. szam"
    |n==0=1
    |otherwise =n*fakt2(n-1)

fakt3 n res
    |n<0=error "neg. szam"
    |n==0=res
    |otherwise=fakt3(n-1)(res*n)
-- az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).
hatvanyX x n
    |n<0 = error "neg. kitevo"
    |otherwise = x**n

hatvanyX2 x n
    |n<0=error "neg. kitevo"
    |otherwise =x^n

hatvanyX3 x n
    |n<0=error "neg. kitevo"
    |n==0=1
    |otherwise =x*hatvanyX3 x (n-1)



--az első n természetes szám negyzetgyökét,
negyzetgyok n = [sqrt i | i <- [1..n]]
--az első n négyzetszámot,\
negyzetszam n = [ i^2 | i <- [1..n]]
--az első n természetes szám köbét,
kobszam n = [i^3 | i <- [1..n]]
--az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemNegyzet n =[i | i <- [1..n], i /=(sqrt i^2)]
--x hatványait adott n-ig,
hatvany x n = [x^i | i <- [1..n]]
--egy szám páros osztóinak listáját,
parosOsztok x = [i | i<- [1..x], mod x i == 0,mod i 2 == 0]

parosOsztok2 x = [i | i<- [1..x], mod x i == 0,even i]
--n-ig a prímszámok listáját,
osztok x = [i|i <-[1..x],mod x i ==0]
primszam x =osztok x == [1,x]
primszamN n = [i |i <-[1..n], primszam i == True]
primszamN2 n = [i| i<-[1..n],primszam i]
    where
        primszamL sn = [1,sn] == osztokL sn
        osztokL sn2 = [j | j<-[1..sn2],mod sn2 j ==0]


--n-ig az összetett számok listáját,
osszetett n = [i|i <- [1..n],primszam i == False]
osszetett2 n = [i|i <- [1..n], not (primszam i)]
--n-ig a páratlan összetett számok listáját,
paratlanOszetett n = [i|i <- [1..n], not (primszam i),mod i 2 /=0]
paratlanOszetett2 n = [i|i <- [1..n], not (primszam i),not (even i)]
paratlanOszetett3 n = [i|i <- [1..n], not (primszam i),odd i]

--az n-nél kisebb Pitágorászi számhármasokat,
pitagorasz n = [(a,b,c) | c<-[1..n],b <- [1..c],a <- [1..b],a^2+b^2==c^2]

--a következő listát: [(a,0),(b,1),…,(z,25)],
betuSzam = zip ['a' .. 'z'][0..]

betuSzam2 = zip ['a' .. 'z'][25,24..]

--a következő listát: [(0,5),(1,4),(2,3),(3,2),(4,1),(5,0)]
--, majd általánosítsuk a feladatot.

szamok = zip [0..5][5,4..0]

szamok2 n = zip [0..n][n,n-1..0]

szamok3 n = [(i,n-i) | i<-[0..n]]

--azt a listát, ami felváltva tartalmaz True és False értékeket.

tf n = take n ls 
    where
        ls = [True,False] ++ ls

tf2 n =[mod i 2 == 0 | i <-[0..n]]

main=do
    putStr "elempar (6,7) (7,6): "
    print (elempar (6,7) (7,6))
    putStrLn ("elempar (6,7) (4,6): " ++ show (elempar2 (6,7) (4,6)))
    print(tf2 10)