{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Aeson
import           Data.Aeson.Utils
import qualified Data.HashMap.Strict as HM
import           Data.Scientific
import           Data.Text
import           Data.Vector

---------------------------------- Parsable


class Parsable x where
  fromValue :: Value -> Maybe x

instance Parsable Double where
  fromValue (Number n) =
    case floatingOrInteger n of
      Left i  -> Just i
      Right i -> Just $ fromInteger i

  fromValue _ = Nothing

instance Parsable Integer where
  fromValue (Number n) =
    case floatingOrInteger n of
      Left _  -> Nothing
      Right i -> Just i

  fromValue _ = Nothing

instance Parsable Text where
  fromValue (String s) = Just s
  fromValue _          = Nothing

instance Parsable a => Parsable [a] where
  fromValue (Array a) = Prelude.sequence $ Data.Vector.toList $ fmap fromValue a
  fromValue _ = Nothing

instance Parsable Bool where
  fromValue (Bool b) = Just b
  fromValue _        = Nothing


-------------------------------------- FromObject



class FromObject o where
  fromObject :: HM.HashMap Text Value -> Maybe o


-------------------------------------- Messages

data Person = Person
            { name    :: Text
            , surname :: Text
            , age     :: Integer
            } deriving (Show)

instance FromObject Person where
  fromObject m = Person
    <$> (fromValue =<< HM.lookup "name" m)
    <*> (fromValue =<< HM.lookup "surname" m)
    <*> (fromValue =<< HM.lookup "age" m)













