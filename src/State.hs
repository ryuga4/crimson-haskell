{-# LANGUAGE ViewPatterns #-}

module State where

import           Data.Map.Strict               as Map
import           Data.Text
import           Network.WebSockets.Connection as WS
import qualified Parser as Parser


data GameState = GameState
               deriving (Show)

data Room = Room
          { roomName  :: Text
          , clients   :: [(Text, WS.Connection)]
          , gameState :: GameState
          }

instance Show Room where
  show r = show (gameState r, [x | (x,_) <- clients r])

instance Eq Room where
  (roomName -> a) == (roomName -> b) = a == b

instance Ord Room where
  compare (roomName -> a) (roomName -> b) = compare a b


newtype State = State
           { rooms :: Map Text Room
           } deriving (Show)

initial :: State
initial = State Map.empty

initialRoom :: Text -> Text -> WS.Connection -> Room
initialRoom r n c = Room r [(n,c)] initialState

addClient :: Room -> Text -> WS.Connection -> Room
addClient r n c = r { clients = (n,c) : clients r
                    , gameState = addPlayer n $ gameState r
                    }

initialState :: GameState
initialState = GameState

addPlayer :: Text -> GameState -> GameState
addPlayer _ state = state



join :: Text -> Text -> WS.Connection -> State -> State
join userRoomName roomRoomName conn (rooms -> rooms') = State newRooms
  where
    f new old = old { clients = c1 ++ c2
                    , gameState = addPlayer userRoomName state
                    }
       where
         c1 = clients new
         c2 = clients old
         state = gameState old

    newRooms = Map.insertWith f roomRoomName (initialRoom roomRoomName userRoomName conn) rooms'

leave :: Text -> Text -> State -> State
leave _ _ state = state

