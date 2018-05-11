import qualified Data.Map as Map
import Data.Maybe

-- 5. úloha
--
-- 1) Definujte datový typ 'Trie k v' reprezentující trii, kde klíče (řetězce)
-- jsou typu '[k]' a hodnoty typu 'v'.

data Trie k v = Trie (Map.Map k (Trie k v)) (Maybe v)
    deriving (Show, Eq)

-- Implementujte následující:
--
--   empty :: Trie k v
--
-- 'empty' je jednoduše konstanta, reprezentující prádznou trii.
--
-- > empty == fromList []

empty :: Trie k v
empty = Trie Map.empty Nothing

--   singleton :: [k] -> v -> Trie k v
--
-- 'singleton ks v' je trie, která obsahuje právě jednen klíč 'ks'
-- s hodnotou 'v'.
--
-- > singleton ks v == fromList [(ks, v)]

singleton :: [k] -> v -> Trie k v
singleton [] v = Trie Map.empty (Just v)
singleton (x:xs) v = Trie (Map.singleton x rest) Nothing
    where rest = singleton xs v

--   insertWith :: (Ord k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
--
-- 'insertWith f ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud
-- trie již klíč 'ks' (s hodnotou 'old') obsahuje, původní hodnota je nahrazena
-- hodnotou 'f new old'.
--
-- > insertWith (++) "a" "x" empty                  == fromList [("a","x")]
-- > insertWith (++) "a" "x" (fromList [("a","y")]) == fromList [("a","xy")]

insertWith :: (Ord k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith f [] new (Trie m old) = Trie m (fmap (f new) old)
insertWith f (k:ks) new (Trie m old) = Trie (Map.insert k rest m) old
    where
        rest = case (Map.lookup k m) of
            Just subt -> insertWith f ks new subt
            Nothing   -> singleton ks new

--   insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
--
-- 'insert ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud trie
-- již klíč 'ks' obsahuje, původní hodnota je nahrazena hodnotou 'new'
--
-- Hint: použijte 'insertWith'
--
-- > insert "a" "x" (fromList [("a","y")]) == fromList [("a","x")]

insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insert k v t = insertWith const k v t

--   find :: (Ord k) => [k] -> Trie k v -> Maybe v
--
-- 'find k t' vrátí hodnotu odpovídající klíči 'k' (jako 'Just v'), pokud
-- existuje, jinak 'Nothing'.
--
-- > find "a" empty                  == Nothing
-- > find "a" (fromList [("a","x")]) == Just "x"

find :: (Ord k) => [k] -> Trie k v -> Maybe v
find [] (Trie _ val) = val
find (k:ks) (Trie m v) = case (Map.lookup k m) of
    Just subt -> find ks subt
    Nothing   -> Nothing

--   member :: (Ord k) => [k] -> Trie k v -> Bool
--
-- 'member k t' zjistí, jestli se klíč 'k' nalézá v trii 't'.
--
-- Hint: použijte 'find'

member :: (Ord k) => [k] -> Trie k v -> Bool
member k t = isJust $ find k t

-- Funkce 'fromList' není nutná, ale může se vám hodit pro testování.
--
--   fromList :: (Ord k) => [([k], v)] -> Trie k v
--
--
-- BONUS) Implementujte funkci
--
--   delete :: (Ord k) => [k] -> Trie k v -> Trie k v
--
-- 'delete ks t' smaže klíč 'ks' (a odpovídající hodnotu) z trie 't', pokud
-- klíč 'ks' není v trii obsažený, 'delete' vrátí původní trii.
--
-- > delete "a" (fromList [("b","y")]) == fromList [("b","y")]
-- > delete "a" (fromList [("a","x")]) == fromList []
