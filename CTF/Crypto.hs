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
