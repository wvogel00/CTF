module PicoCTF2019.Crypto.Easy1 where

import CTF.Crypto
import Data.List

filepath = "PicoCTF2019/Crypto/Easy1/table.txt"
encrypted = "UFJKXQZQUNB"
key       = "SOLVECRYPTO"

newtype Key = Key {unKey :: String} deriving (Eq, Show, Ord)
newtype Encrypted = Encrypted {unEncrypted :: String} deriving (Eq, Show, Ord)
newtype Decrypted = Decrypted {unDecrypted :: String} deriving (Eq, Show, Ord)

main = do
	xs <- makeTable <$> readFile filepath
	print.unDecrypted $ solve (Key key) (Encrypted encrypted) xs

solve :: Key -> Encrypted -> KeyTable -> Decrypted
solve (Key ks) (Encrypted es) table = Decrypted $ decryptFromTable ks es table
