module Helper where

import Data.Aeson



valueToObject :: Value -> Either String Object
valueToObject (Object o) = Right o
valueToObject _          = Left "not an object"

maybeToEither :: s -> Maybe a -> Either s a
maybeToEither s Nothing  = Left s
maybeToEither _ (Just a) = Right a


(|$>) :: Applicative f => f a -> (a -> b) -> f b
(|$>) = flip (<$>)

infixl 1 |$>




----------------------------- Useles


mDown :: Maybe (Maybe a) -> Maybe a
mDown Nothing        = Nothing
mDown (Just Nothing) = Nothing
mDown (Just x)       = x

mapMToM :: Monad m => m a -> (a -> b) -> (b -> m c) -> m c
mapMToM a b c = a |$> b >>= c

