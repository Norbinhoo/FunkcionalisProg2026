import Data.List        

mySum ls = foldl op 0 ls
    where
        op res k =res+k

mySum2 ls = foldl1 op ls
    where
        op res k =res+k

mySum3 ls = foldl1 (+) ls

myMap fg ls = foldr (op fg) [] ls
    where
        op fg res k = fg k : res

myMap_ fg ls = foldl (op fg) [] ls
    where
        op fg res k = fg k : res

myFilter fg ls = foldr (op fg) [] ls
    where
        op fg k res = if fg k then k : res else res

myElem x ls = foldr (op x) False ls
    where
        op x k res = if x == k then True || res else res

myElem_ fg ls = foldl (op fg) False ls
    where
        op x k res = if x == k then True || res else res

ls :: [(String, String)]
ls = [("Mari", "mari@gmail.com"), ("Zoli", "zoli@ms.sapientia.ro"),
      ("Teri", "teri@yahoo.com"), ("Bori", "bori@gmail.gom")]

lsR :: [(String, String)]
lsR = [("Bori", "bori@gmail.gom"), ("Mari", "mari@gmail.com"), ("Zoli", "zoli@ms.sapientia.ro")]

ins :: (t -> t -> Bool) -> t -> [t] -> [t]
ins fg k [] = [k]
ins fg x (k : ve)
    | fg x k = k : ins fg x ve
    | otherwise = x : k : ve

fgIns :: Ord a => (a, b1) -> (a, b2) -> Bool
fgIns x y = fst x < fst y

fibonacciLs2 n =reverse $ fst $ foldl op ([],(0,1)) [1..n]
    where
        op (ls,(a,b)) k = (a:ls,(b,a+b))

myMax ls = foldl op [head ls] ls
    where
        op res k 
            |k3 > r3 = [k]
            |k3 == r3 = k:res
            |otherwise = res
                where
                    (r1,r2,r3) =head res
                    (k1,k2,k3) = k