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
-- az n szám faktoriálisát (3 módszer),
-- az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).