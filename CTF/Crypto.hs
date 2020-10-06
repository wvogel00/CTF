module CTF.Crypto where

import Data.Char

caesar :: Int -> String -> String
caesar k = map (\c -> if isAlpha c then caesar' c else c)
	where
	caesar' c
		| isUpper c = chr $ 65 + mod (ord c - 65 + k) 26
		| isLower c = chr $ 97 + mod (ord c - 97 + k) 26
		| isDigit c = chr $ 48 + mod (ord c - 48 + k) 10
		| otherwise = c
