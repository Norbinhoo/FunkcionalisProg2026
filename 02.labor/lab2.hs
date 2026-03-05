import Control.Monad.Trans.Cont (reset)
import Graphics.Win32 (eWX_LOGOFF)
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

szjOsszeg n
    | n < 0 = szjOsszeg(abs n)
    | n < 10 = n
    | otherwise = mod n 10 + szjOsszeg(div n 10)

szjOsszeg2 n res
    | n < 0 = szjOsszeg2(abs n) res
    | n < 10 = res+n
    |otherwise = szjOsszeg2(div n 10) (res + mod n 10)
-- egy szám számjegyeinek számát (2 módszerrel),

szjSzama n res
    | n < 0 = szjSzama(abs n) res
    | n < 10 = res +1
    | otherwise =  szjSzama(div n 10)(res +1)

szjSzama2 n
    | n < 0 = szjSzama2(abs n)
    | n < 10 = 1
    |otherwise =1+szjSzama2(div n 10)
-- egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:

--  ```haskell
--  > fugv4 577723707 7
--  35
--  ```

szjSzamOsszeg n szj
    | szj > 9 = error "nem szamjegy"
    |n<10=if n == szj then szj else 0
    | otherwise = if mod n 10 == szj then szj + szjSzamOsszeg(div n 10) szj else szjSzamOsszeg(div n 10) szj

szjSzamOsszeg2 n szj elof
    | szj > 9   = error "nem szamjegy"
    | n < 10    = if n == szj then (elof + 1) * szj else elof * szj
    | otherwise = if mod n 10 == szj 
                  then szjSzamOsszeg2 (div n 10) szj (elof + 1) 
                  else szjSzamOsszeg2 (div n 10) szj elof

-- egy szám páros számjegyeinek számát,

parosSzamSzj n
    | n<0=parosSzamSzj(abs n)
    |n<10=if even n then 1 else 0
    |otherwise=if even (mod n 10)
                    then 1 + parosSzamSzj(div n 10)
                    else parosSzamSzj(div n 10)

parosSzamSzj2 n res
    |n<0=parosSzamSzj2(abs n) res
    |n<10 =if even n then res+1 else res
    |otherwise=if even (mod n 10)
                then parosSzamSzj2(div n 10)(res +1)
                else parosSzamSzj2(div n 10) res
-- egy szám legnagyobb számjegyét,
legSzj n ln
    |n<0=legSzj(abs n) ln
    |n<10=if n > ln then n else ln --vagy n<10 max n ln
    |otherwise =
        if mod n 10>ln 
            then legSzj(div n 10) (mod n 10)
            else legSzj(div n 10) ln

-- egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
 -- Példák függvényhívásokra:

  --```haskell
  --fugv 7673573 10 7 -> 3
  --fugv 1024 2 1 -> 1
  --fugv 1023 2 1 -> 10
  --fugv 345281 16 4 -> 2
  --```

bSzamrDSzj n b d
    |n<0=bSzamrDSzj(abs n) b d
    |n<b=if n == d then 1 else 0
    |otherwise = 
        if mod n b== d 
            then 1 + bSzamrDSzj(div n b) b d
            else bSzamrDSzj(div n b) b d

-- az 1000-ik Fibonacci számot.

fibo a b res n 
    | n ==0=res
    |otherwise=fibo b res (res+b)(n-1)

fiboN n=fibo 0 1 0 n

fiboN2 n =fiboSg 0 1 0 n
    where
        fiboSg _ _ res 0=res
        fiboSg a b res n = fiboSg b res(res+b)(n-1)
fiboSzamok n = map fiboN[0..n]

fiboN3 n = fiboSzamok n !! n

--II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.
ls1=[234,64,12,9,0]

szjSzorzatLs ls =map szjSzorzat2 ls

szjSzorzatLs2 ls = map(\x -> (x,szjSzorzat2 x)) ls

szjOsszegLs = map szjOsszeg ls1

szjOsszegLs2 ls =map (\x -> szjOsszeg2 x 0) ls

szjSzamLs ls = map szjSzama2 ls

ls2=[(577723707,7),(423,3),(0,1),(12,2)]
szjSzamOsszegLs ls = map (uncurry szjSzamOsszeg) ls

szjSzamOsszegLs2 ls =map (\(x,szj) -> szjOsszeg x szj) ls

legSzjLs ls =map (\x -> legSzj x 0) ls


ls3 = [(7673573,10,7),(1024,2,1),(1023,2,1),(345281,16,4)]

bSzamrDSzjLs = map(\(n,b,d) -> bSzamrDSzj n b d) ls3

