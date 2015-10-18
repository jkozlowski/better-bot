{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Types
-- Copyright   :  (c) 2015, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Contains config types.
-----------------------------------------------------------------------------
module Types where

import           Control.Lens.TH      (makeClassy, makePrisms)
import           Data.Aeson
import qualified Data.Aeson.TH        as A
import qualified Data.Map.Strict      as M
import           Data.Text            (Text, pack)
import qualified Data.Yaml            as Yaml
import           Network.Better.Aeson (decapitalizeJsonOptionsRemovePrefix)

data DayOfWeek
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Eq, Show, Read, Enum, Ord)

$(makeClassy ''DayOfWeek)
$(makePrisms ''DayOfWeek)
$(A.deriveJSON A.defaultOptions ''DayOfWeek)

newtype Slots = Slots (M.Map DayOfWeek Text)
  deriving (Eq, Show)

$(makeClassy ''Slots)
$(makePrisms ''Slots)

instance Yaml.ToJSON Slots where
   toJSON (Slots m) = object   .
                      M.assocs .
                      M.mapKeys (pack . show) .
                      M.map String $ m

instance Yaml.FromJSON Slots where
  parseJSON v@(Object _) = Slots . M.mapKeys read <$> parseJSON v
  parseJSON _ = mempty

data Booking = Booking
  { _bookingFacility     :: {-# UNPACK #-} !Text
  , _bookingActivityType :: {-# UNPACK #-} !Text
  , _bookingActivity     :: {-# UNPACK #-} !Text
  , _bookingEmail        :: {-# UNPACK #-} !Text
  , _bookingPassword     :: {-# UNPACK #-} !Text
  , _bookingSlots        ::                !Slots
  } deriving (Eq, Show)

$(makeClassy ''Booking)
$(makePrisms ''Booking)
$(A.deriveJSON (decapitalizeJsonOptionsRemovePrefix "_booking") ''Booking)

data Config = Config
  { _configBookings :: ![Booking]
  } deriving (Eq, Show)

$(makeClassy ''Config)
$(makePrisms ''Config)
$(A.deriveJSON (decapitalizeJsonOptionsRemovePrefix "_config") ''Config)

readConfig :: IO (Either Yaml.ParseException Config)
readConfig = Yaml.decodeFileEither "config.yaml"
