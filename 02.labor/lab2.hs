-- egy szám számjegyeinek szorzatát (2 módszerrel),

szjSzorzat 0 = 1
szjSzorzat n = mod n 10 * szjSzorzat (div n 10)

szjSzorzat2 n 
    |n < 0 = error "neg. szam"
    |div n 10 == 0 = n
    |otherwise = mod n 10 * szjSzorzat2 (div n 10)

szjSzorzat3 n res 
    |n < 0 = error "neg. szam"
    |div n 10 == 0 = res * n
    |otherwise = szjSzorzat3 (div n 10) (res *(mod n 10))
-- egy szám számjegyeinek összegét (2 módszerrel),
-- egy szám számjegyeinek számát (2 módszerrel),
-- egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:

--  ```haskell
--  > fugv4 577723707 7
--  35
--  ```