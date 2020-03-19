module Graphics 
    ( windowHeight
    , windowWidth
    , dot
    , render
    , Image
    , Color(..)
    , Pos(..)
    ) where

import Control.Monad.Free
import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Data.Color as G
import qualified Data.Map.Lazy as M

data Pixel = Pixel { unwrap :: G.Picture }

data Color = Color Float Float Float deriving Show
data Pos = Pos Float Float deriving (Show, Ord, Eq)

windowWidth, windowHeight :: Num n => n
windowWidth = 640
windowHeight = 480

pixel :: Pos -> Color -> Pixel
pixel (Pos x y) (Color r g b) = let
    x' = x - windowWidth/2
    y' = y - windowHeight/2
    c' = G.makeColor r g b 1
    geo = [(x'+0, y'+0), (x'+0, y'+1), (x'+1, y'+1), (x'+1, y'+0)]
    in Pixel . G.color c' $ G.polygon geo

type Image = Free Image'

data Image' r = Dot Pos Color r
             deriving Show

instance Functor Image' where
    fmap f (Dot p c r) = Dot p c (f r)

dot :: Pos -> Color -> Image ()
dot p c = liftF $ Dot p c ()

render :: Free Image' r -> IO ()
render = display . space2pixels . render' initLightSpace
    where
    initLightSpace = mempty
    -- initLightSpace :: M.Map Pos Color
    -- initLightSpace = M.fromList [(Pos x y, Color 0 0 0)|
    --     x <- [0..windowHeight-1], y <- [0..windowWidth-1]]

    render' :: M.Map Pos Color -> Free Image' r -> M.Map Pos Color
    render' m (Free (Dot p c r)) = render' (M.insert p c m) r
    render' m (Pure _) = m

    space2pixels :: M.Map Pos Color -> [Pixel]
    space2pixels = map (uncurry pixel) . M.toList

    display :: [Pixel] -> IO ()
    display writes = G.display window G.white (G.pictures $ map unwrap writes)
        where
        window :: G.Display
        window = G.InWindow "ray-tracing" (windowWidth, windowHeight) (100, 100)

