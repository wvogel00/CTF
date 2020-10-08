module Caesar where

import CTF.Crypto

filepath = "PicoCTF2019/Crypto/caesar/ciphertext"

-- answer is acquired by calling the function as : "caesar 11 str"
main = do
    encrypted <- readFile filepath
    mapM_ print $ map (flip caesar encrypted) [0..26]
