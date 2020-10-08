module PicoCTF2019.Crypto.Tapping where

import CTF.Crypto

signals = ".--. .. -.-. --- -.-. - ..-. { -- ----- .-. ... ...-- -.-. ----- -.. ...-- .---- ... ..-. ..- -. .---- -.... --... --... ..--- ..... --... ..--- ---.. --... }"

main = do
	print $ map demorse $ map (map toMorse).filter (not.flip elem ["{","}"]) $ words signals

toMorse '.' = Dot
toMorse '-' = Dash
