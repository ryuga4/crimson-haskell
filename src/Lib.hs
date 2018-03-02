{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Concurrent
import qualified Control.Concurrent.STM.TVar   as STM
import qualified Control.Exception             as Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.STM             as STM
import           Data.Aeson
import qualified Data.HashMap.Strict           as HM
import           Data.Text
import           GHC.Int
import           Helper
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.WebSockets.Connection as WS
import qualified Parser                        as Prs
import           Servant
import qualified Servant.API.WebSocket         as WS
import           State                         as St
import qualified System.Clock                  as C

------------------------------------- Config
minFps = 1
maxFps = 10000


-------------------------------------
type API = "ws" :> QueryParam "user" Text :> QueryParam "room" Text :> WS.WebSocket


startApp :: IO ()
startApp = do
  state <- STM.newTVarIO St.initial
  putStrLn "Starts on 8080"
  forkIO $ loop 60 state
  run 8080 $ app state

app :: STM.TVar St.State -> Application
app state = serve api $ server state

api :: Proxy API
api = Proxy

server ::  STM.TVar St.State -> Server API
server state = streamData
 where
  streamData :: MonadIO m => Maybe Text -> Maybe Text -> WS.Connection -> m ()
  streamData Nothing _ _ = do
    liftIO $ putStrLn "Wrong connection, no name"
    return ()
  streamData _ Nothing _ = do
    liftIO $ putStrLn "Wrong connection, no room"
    return ()

  streamData (Just n) (Just r) c = liftIO $ do
    putStrLn ("User " ++ show n ++ " joined room " ++ show r ++ "") -- info
    WS.forkPingThread c 30 -- maintain connection

    STM.atomically
      (STM.modifyTVar state (St.join n r c))
      >> STM.readTVarIO state >>= print

    Exception.finally
      (listen c state) -- listen
      $ STM.atomically (STM.modifyTVar state (St.leave n r)) -- finally leave
      >> putStrLn ("User " ++ show n ++ " left room " ++ show r ++ "") -- info state


timeSpan (C.TimeSpec {C.sec=s1, C.nsec=n1}) (C.TimeSpec {C.sec=s2, C.nsec=n2}) = n2-n1 + (1000000000 * (s2-s1))

measureTime :: IO a -> IO Int64
measureTime a = liftM3 (\s _ e -> timeSpan s e) (C.getTime C.Monotonic) a (C.getTime C.Monotonic)


update :: Int -> St.State -> St.State
update fps s = s

send :: Int -> St.State -> IO ()
send fps s = do
  threadDelay 1000
  return ()


loop :: Int -> STM.TVar St.State -> IO ()
loop fps state = do
    time <- measureTime $ do
      print fps
      STM.readTVarIO state >>= send fps
      STM.atomically $ STM.modifyTVar' state (update fps)
    let frameDuration = 1000000 `div` fps
    let operationTime = fromIntegral time `div` 1000
    print (frameDuration, operationTime)
    threadDelay (frameDuration - operationTime)
    let newFps' | operationTime * 10 > frameDuration * 8 = fps-1
                | operationTime * 10 < frameDuration * 2 = fps+1
                | otherwise = fps
        newFps | newFps' < minFps = minFps
               | newFps' > maxFps = maxFps
               | otherwise = newFps'
    loop newFps state



listen :: WS.Connection -> STM.TVar St.State -> IO ()
listen conn state = forever $ do
  msg <- WS.receiveData conn |$> (eitherDecode >=> valueToObject)
  let body  = msg >>= maybeToEither "Bad body" . HM.lookup "body"
  let topic = msg >>= maybeToEither "Bad topic" . (HM.lookup "topic" >=> Prs.fromValue)
  route' state topic body conn

route' :: STM.TVar St.State -> Either String Text -> Either String Value -> WS.Connection -> IO ()
route' _ (Left s)      _         _ = print s
route' _ _             (Left s)  _ = print s
route' _ (Right topic) (Right _) _ =
  case topic of
    "add_player" -> putStrLn "add player"
    _            -> putStrLn "jakaś dziwna wiadomość"
