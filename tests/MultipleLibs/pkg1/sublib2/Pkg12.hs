module Pkg12 where

import qualified Pkg11 ( me )

me :: String
me = take 4 Pkg11.me ++ "2"
