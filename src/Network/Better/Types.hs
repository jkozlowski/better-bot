
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Better.Types
-- Copyright   :  (c) 2015, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Contains types for interacting with the API.
-----------------------------------------------------------------------------
module Network.Better.Types (
   Booking, _Booking, emptyBooking
 , bookingParentId, bookingParentTitle, bookingName
 , bookingId, bookingRemainingSlots, bookingInstructor
 , bookingLocation, bookingFacility, bookingFacilityId
 , bookingDate, bookingStartTime, bookingEndTime
 , bookingCanCancel, bookingStatus, bookingCanSelectCourt

 , FacilityId(..), Facility, _Facility, emptyFacility
 , facilityName, facilityId, facilityIsHomeClub, facilityLocationId

 , BasketCount, _BasketCount, emptyBasketCount
 , basketBasketCount

 , ActivityTypeId(..), ActivityType, _ActivityType, emptyActivityType
 , activityTypeName, activityTypeId, activityTypeFacilityId

 , ActivityId(..), Activity, _Activity, emptyActivity
 , activityName, activityId, activityActivityTypeId

 , TimetableEntryId(..), TimetableEntry, _TimetableEntry, emptyTimetableEntry
 , timetableEntryParentId, timetableEntryId, timetableEntryRemainingSlots
 , timetableEntryDate, timetableEntryStartTime, timetableEntryEndTime

 , BasketItemId(..), BasketItem, _BasketItem, emptyBasketItem
 , basketItemId, basketItemAllocateBookingCreditUrl
 ) where

import Control.Lens         ( makeClassy, makePrisms )
import Data.Aeson           ( FromJSON(..), ToJSON(..), (.:)
                            , Value(Object), object, pairs, (.=))
import Data.Aeson.TH        ( deriveJSON, defaultOptions, fieldLabelModifier )
import Data.Monoid          ( (<>) )
import Data.Text            ( Text )
import Network.Better.Aeson ( jsonOptions, decapitalizeJsonOptions
                            , jsonOptionsRemovePrefix )

-- | User's booking.
data Booking = Booking
  { _bookingParentId          :: Int
  , _bookingParentTitle       :: Maybe Text
  , _bookingName              :: Text
  , _bookingId                :: Int
  , _bookingRemainingSlots    :: Int
  , _bookingInstructor        :: Text
  , _bookingLocation          :: Text
  , _bookingFacility          :: Text
  , _bookingFacilityId        :: Int
  , _bookingDate              :: Text -- Change to Day
  , _bookingStartTime         :: Text -- Change to Time
  , _bookingEndTime           :: Text -- Change to Time
  , _bookingCanCancel         :: Bool
  , _bookingStatus            :: Text -- Change to some enum maybe
  -- , _bookingExcerciseCategory :: Maybe Text -- Don't actually know what that is
  , _bookingCanSelectCourt    :: Bool
  -- , _bookingDescription       :: Maybe Text
  -- , _bookingItems             :: [] -- Not sure what this is
  } deriving (Show, Eq)

$(makeClassy ''Booking)
$(deriveJSON jsonOptions ''Booking)
$(makePrisms ''Booking)

emptyBooking = Booking
  { _bookingParentId       = 0
  , _bookingParentTitle    = Nothing
  , _bookingName           = ""
  , _bookingId             = 0
  , _bookingRemainingSlots = 0
  , _bookingInstructor     = ""
  , _bookingLocation       = ""
  , _bookingFacility       = ""
  , _bookingFacilityId     = 0
  , _bookingDate           = ""
  , _bookingStartTime      = ""
  , _bookingEndTime        = ""
  , _bookingCanCancel      = False
  , _bookingStatus         = ""
  , _bookingCanSelectCourt = False
  }

-- | Id of Facility.
newtype FacilityId = FacilityId Int
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Facility details.
data Facility = Facility
  { _facilityName       :: Text
  , _facilityId         :: FacilityId
  , _facilityIsHomeClub :: Bool
  , _facilityAddress1   :: Maybe Text
  , _facilityAddress2   :: Maybe Text
  , _facilityAddress3   :: Maybe Text
  , _facilityCity       :: Maybe Text
  , _facilityPostCode   :: Maybe Text
  , _facilityPhone      :: Maybe Text
  , _facilityLocationId :: Int
  } deriving (Show, Eq)

$(makeClassy ''Facility)
$(deriveJSON jsonOptions ''Facility)
$(makePrisms ''Facility)

emptyFacility = Facility
  { _facilityName       = ""
  , _facilityId         = FacilityId 0
  , _facilityIsHomeClub = False
  , _facilityAddress1   = Nothing
  , _facilityAddress2   = Nothing
  , _facilityAddress3   = Nothing
  , _facilityCity       = Nothing
  , _facilityPostCode   = Nothing
  , _facilityPhone      = Nothing
  , _facilityLocationId = 0
  }

-- | Count of items in the basket.
data BasketCount = BasketCount
  { _basketBasketCount  :: Int
  } deriving (Show, Eq)

$(makeClassy ''BasketCount)
$(deriveJSON decapitalizeJsonOptions ''BasketCount)
$(makePrisms ''BasketCount)

emptyBasketCount = BasketCount
  { _basketBasketCount = 0
  }

-- | Id of ActivityType.
newtype ActivityTypeId = ActivityTypeId Int
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Type of activity in a facility.
data ActivityType = ActivityType
  { _activityTypeName       :: Text
  , _activityTypeId         :: ActivityTypeId
  , _activityTypeFacilityId :: FacilityId
  } deriving (Show, Eq)

$(makeClassy ''ActivityType)
$(deriveJSON (jsonOptionsRemovePrefix "_activityType") ''ActivityType)
$(makePrisms ''ActivityType)

emptyActivityType = ActivityType
  { _activityTypeName       = ""
  , _activityTypeId         = ActivityTypeId 0
  , _activityTypeFacilityId = FacilityId 0
  }

-- | Id of specific Activity.
newtype ActivityId = ActivityId Int
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Specific activity.
data Activity = Activity
  { _activityName           :: Text
  , _activityId             :: ActivityId
  , _activityActivityTypeId :: ActivityTypeId
  } deriving (Show, Eq)

$(makeClassy ''Activity)
$(deriveJSON (jsonOptionsRemovePrefix "_activity") ''Activity)
$(makePrisms ''Activity)

emptyActivity = Activity
  { _activityName           = ""
  , _activityId             = ActivityId 0
  , _activityActivityTypeId = ActivityTypeId 0
  }

-- | Id of TimetableEntry.
newtype TimetableEntryId = TimetableEntryId Int
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Entry in a timetable.
data TimetableEntry = TimetableEntry
  { _timetableEntryParentId       :: Int
  -- , _timetableEntryParentTitle      :: Maybe Text
  -- , _timetableEntryName             :: Text
  , _timetableEntryId             :: TimetableEntryId
  , _timetableEntryRemainingSlots :: Int
  -- , _timetableEntryInstructor       :: Maybe Text
  -- , _timetableEntryLocation         :: Text
  -- , _timetableEntryFacility         :: Text
  -- , _timetableEntryFacilityIt       :: FacilityId
  , _timetableEntryDate           :: Text
  , _timetableEntryStartTime      :: Text
  , _timetableEntryEndTime        :: Text
  -- , _timetableEntryCanCancel        :: Bool
  -- , _timetableEntryStatus           :: Maybe Text
  -- , _timetableEntryExerciseCategory :: Maybe Text
  -- , _timetableEntryCanSelectCourt   :: Bool
  -- , _timetableEntryDescription      :: Maybe Text
  -- , _timetableEntryItems            :: [Text]
  } deriving (Show, Eq)

$(makeClassy ''TimetableEntry)
$(deriveJSON (jsonOptionsRemovePrefix "_timetableEntry") ''TimetableEntry)
$(makePrisms ''TimetableEntry)

emptyTimetableEntry = TimetableEntry
  { _timetableEntryParentId       = 0
  , _timetableEntryId             = TimetableEntryId 0
  , _timetableEntryRemainingSlots = 0
  , _timetableEntryDate           = ""
  , _timetableEntryStartTime      = ""
  , _timetableEntryEndTime        = ""
  }

-- | Id of BasketItem.
newtype BasketItemId = BasketItemId Int
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Item in a basket.
data BasketItem = BasketItem
  { _basketItemId                       :: BasketItemId
  , _basketItemAllocateBookingCreditUrl :: Text
  } deriving (Show, Eq)

$(makeClassy ''BasketItem)
$(deriveJSON (jsonOptionsRemovePrefix "_basketItem") ''BasketItem)
$(makePrisms ''BasketItem)

emptyBasketItem = BasketItem
  { _basketItemId       = BasketItemId 0
  , _basketItemAllocateBookingCreditUrl = ""
  }
