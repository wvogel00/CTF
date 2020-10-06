module PicoCTF2019.Crypto.Q13 where

import CTF.Crypto
import Data.List

crypted = "cvpbPGS{abg_gbb_onq_bs_n_ceboyrz}"

main = mapM_ print $
	filter (isPrefixOf "picoCTF") $ (map (flip caesar crypted)) [-26..26]
