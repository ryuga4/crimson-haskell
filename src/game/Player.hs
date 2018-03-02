module Game.Player where

import Data.Text
import qualified Network.WebSockets.Connection as WS






data Player = Player
            { name :: Text
            , conn :: WS.Connection
            , pos  :: (Float, Float)
            , hp   :: Int
            , speed :: Float
            , angle :: Double
            } deriving (Show)

instance Show WS.Connection where
 show _ = "Connection"



movePlayer ::  Float -> Float -> Player -> Player
movePlayer x' y' p@Player { pos = (x,y), speed = s } = p { pos = (x+x'*s,y+y'*s) }



moveLeft :: Player -> Player
moveLeft = movePlayer (-1) 0

moveRight :: Player -> Player
moveRight = movePlayer 1 0

moveUp :: Player -> Player
moveUp = movePlayer 0 1

moveDown :: Player -> Player
moveDown = movePlayer 0 (-1)

takeHit :: Int -> Player -> Player
takeHit x p@Player { hp = hp' } = p { hp = hp' - x}

heal :: Int -> Player -> Player
heal x p@Player { hp = hp' } = p { hp = hp' + x}

