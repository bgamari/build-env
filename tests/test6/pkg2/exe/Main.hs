module Main where

import qualified Pkg1  ( me )
import qualified Pkg11 ( me )
import qualified Pkg12 ( me )
import qualified Pkg2  ( me )

main :: IO ()
main = print [ Pkg1.me, Pkg11.me, Pkg12.me, Pkg2.me ]
