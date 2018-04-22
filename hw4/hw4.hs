import Data.Char

-- 4. úloha
--
-- 1) Implementuje následující (tři) funkce:
--
--   on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
--
-- on f g  aplikuje g na oba dva argumenty a pak aplikuje f.
--
-- > (max `on` abs) (-5) 4
-- 5
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a b = f (g a) (g b) 
--   while :: (a -> Bool) -> (a -> a) -> a -> a
--
-- while c f  opakovaně aplikuje funkci f na svůj argument a to dokud platí
-- podmínka daná funkcí c.
--
-- > while (<100) (*2) 1
-- 128
while :: (a -> Bool) -> (a -> a) -> a -> a
while c f x
    | c x       = while c f (f x)
    | otherwise = x
--   pairwise :: (a -> a -> a) -> [a] -> [a]
--
-- pairwise f  aplikuje funkci f na dva po sobě jdoucí prvky seznamu,
-- všechny výsledné hodnoty shromáždí do nového seznamu. Pokud má seznam lichou
-- délku, poslední prvek zůstává nezměněn.
--
-- > pairwise (+) [1..9]
-- [3,7,11,15,9]
pairwise :: (a -> a -> a) -> [a] -> [a]
pairwise f [] = []
pairwise f (x1:[]) = [x1]
pairwise f (x1:x2:xs) = (f x1 x2):(pairwise f xs)

--
-- 2) Implementujte mergesort a použijte ho pro case-insensitive třídění
-- řetězců.
--
--   mergeWith :: (a -> a -> Bool) -> [a] -> [a] -> [a]
--   sortWith  :: (a -> a -> Bool) -> [a] -> [a]
--
-- Prvním argumentem je funkce, která provádí porovnávání.
--
-- > sortWith (<) [10,9..1]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- > sortWith (>) [10,9..1]
-- [10,9,8,7,6,5,4,3,2,1]

mergeWith :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeWith c l [] = l
mergeWith c [] r = r
mergeWith c (l:lx) (r:rx)
    | c l r     = l:(mergeWith c lx (r:rx))
    | otherwise = r:(mergeWith c (l:lx) rx)

sortWith  :: (a -> a -> Bool) -> [a] -> [a]
sortWith c [] = []
sortWith c (x:[]) = [x]
sortWith c a = mergeWith c l r
    where
        halfN = (length a) `div` 2
        l = sortWith c $ take halfN a
        r = sortWith c $ drop halfN a

-- Pro case-insensitive třídění se vám může hodit funkce toLower :: Char -> Char
-- z modulu Data.Char. Abyste tuto funkci mohli použít, napište na začátek
-- souboru 'import Data.Char' (viz tohle zadání).
--
--   ciSort :: [String] -> [String]
--
-- > ciSort ["Sort", "me"]
-- ["me","Sort"]
ciSort :: [String] -> [String]
ciSort a = sortWith ciComp a
    where ciComp = (<) `on` (map toLower)

compAbs :: (Ord a, Num a) => a -> a -> Bool
compAbs x y = abs x < abs y

-- > sortWith compAbs [-10,-7..8]
-- [-1,2,-4,5,-7,8,-10]



--
-- BONUS)
--
-- Implementujte sortWith bez použití rekurze. Mohou se vám hodit funkce
-- probírané na cvičení.
--
-- mergeWith může být rekurzivně definovaný.

-- Doufám, že se tohle počítá jako bez použití rekurze (schová se do while).
-- while by šel ještě nějak přepsat pomocí let/fix.
sortWith2  :: (a -> a -> Bool) -> [a] -> [a]
sortWith2 c [] = []
sortWith2 c (x:[]) = [x]
sortWith2 c a = head res
    where
        wrappedA = map (\x -> [x]) a
        step = pairwise (mergeWith c)
        res = while ((>1).length) step wrappedA
