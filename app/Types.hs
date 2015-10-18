{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.Aeson.TH        as A
import qualified Data.Map.Strict      as M
import           Data.Text            (Text)
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
  deriving (Eq, Show)

$(makeClassy ''DayOfWeek)
$(makePrisms ''DayOfWeek)
$(A.deriveJSON A.defaultOptions ''DayOfWeek)

data Slot = Slot
  { _slotDay  ::                !DayOfWeek
  , _slotTime :: {-# UNPACK #-} !Text
  } deriving (Eq, Show)

$(makeClassy ''Slot)
$(makePrisms ''Slot)
$(A.deriveJSON (decapitalizeJsonOptionsRemovePrefix "_slot") ''Slot)

data Booking = Booking
  { _bookingFacility     :: {-# UNPACK #-} !Text
  , _bookingActivityType :: {-# UNPACK #-} !Text
  , _bookingActivity     :: {-# UNPACK #-} !Text
  , _bookingEmail        :: {-# UNPACK #-} !Text
  , _bookingPassword     :: {-# UNPACK #-} !Text
  , _bookingSlots        ::                ![Slot]
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
