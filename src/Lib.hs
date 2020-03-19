module Lib where

import qualified Graphics as G

main :: IO ()
main = G.render $ do
    mapM_ (\x -> G.dot (G.Pos x $ (x-300)*(x-300)/150) red) [0, 0.1..680]
    mapM_ (\x -> G.dot (G.Pos x $ 100 + 5 * sin (x/10)) red) [0..680]
    mapM_ (\x -> G.dot (G.Pos x $ 100 + x/4) red) [0..680]
    where red = G.Color 0.3 0.1 0.1
