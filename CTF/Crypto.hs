module CTF.Crypto where

import Data.Char

caesar :: Int -> String -> String
caesar k = map (\c -> if isAlphaNum c then caesar' c else c)
	where
	caesar' c
		| isUpper c = chr $ 65 + mod (ord c - 65 + k) 26
		| isLower c = chr $ 97 + mod (ord c - 97 + k) 26
		| isDigit c = chr $ 48 + mod (ord c - 48 + k) 10
		| otherwise = c

-----------------------------------------------------------------
-----------------------------------------------------------------
type TableKey = Char
type TableOriginal = Char
type TableEncrypto = Char
type TableDecrypto = Char
type KeyTable = [(TableKey,TableOriginal,TableEncrypto)]


makeTable :: String -> KeyTable
makeTable str = concatMap (makeTable' t) ts
	where
	(t:ts) = filter (not.null).map (filter isAlpha) $ lines str
	makeTable' :: [Char] -> [Char] -> [(TableKey,TableOriginal,TableEncrypto)]
	makeTable' os (k:es) = [(k,snd o,es!!fst o) | o <- zip [0..] os]

decryptFromTable :: [TableKey] -> [TableEncrypto] -> KeyTable -> [TableDecrypto]
decryptFromTable ks es table = map f $ zip ks es
	where
	f (k,e) =  snd3.head $ filter (\(a,b,c) -> a==k&&c==e) table
	snd3 (_,a,_) = a

-----------------------------------------------------------------
-----------------------------------------------------------------

data Tree a = Nil | Tree (Tree a) a (Tree a) deriving Show
data MorseSignal = Dash | Dot deriving Eq

morseTree :: Tree Char
morseTree = makeTree " ETIANMSURWDKGOHVF L PJBXCYZQ  54 3   2  +    16=/     7   8 90"

makeTree :: [a] -> Tree a
makeTree xs = foldl insertTree Nil xs

insertTree tree v = insert tree v
	where
	insert Nil v = Tree Nil v Nil
	insert (Tree Nil x tr) v = Tree (insert Nil v) x tr
	insert (Tree tl x tr)  v = if sizeTree tl == sizeTree tr || sizeTree tl < sum (map (2^) [0..heightTree tl-1])
								then Tree (insert tl v) x tr
								else Tree tl x (insert tr v)

heightTree :: Tree a -> Int
heightTree Nil = 0
heightTree (Tree tl _ tr) = max (1+heightTree tl) (1+heightTree tr)

sizeTree :: Tree a -> Int
sizeTree Nil = 0
sizeTree (Tree tl _ tr) = 1 + sizeTree tl + sizeTree tr

-- モールスを復号
demorse :: [MorseSignal] -> Char
demorse xs = demorse' morseTree xs
	where
	demorse' Nil _ = undefined
	demorse' (Tree _ x _) [] = x
	demorse' (Tree tl _ tr) (x:xs) = if x == Dot then demorse' tl xs else demorse' tr xs

-----------------------------------------------------------------
-----------------------------------------------------------------
