{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BookSlot
-- Copyright   :  (c) 2015, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Logic for booking a slot.
-----------------------------------------------------------------------------
module BookSlot where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Data.List.Lens
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (Day)
import           Network.Better.Session
import           Network.Better.Types
import qualified Network.Wreq.Session   as S
import           System.Exit            (exitFailure)
import           Types

type MonadIOLog m = (MonadIO m, MonadLogger m)

bookActivity :: (MonadThrow m, MonadIOLog m)
             => S.Session
             -> Config
             -> Day
             -> Text
             -> m ()
bookActivity s c date' startTime'
 = do
      let facilityName'     = c ^. configFacility
          activityTypeName' = c ^. configActivityType
          activityName'     = c ^. configActivity
          email             = c ^. configEmail
          bookingMsg = activityName'                                 <>
                       " on "     <> (T.pack . show $ date')         <>
                       " at "     <> startTime'                      <>
                       " for "    <> email                           <>
                       " in "     <> facilityName'

      $(logInfo) $ "Booking " <> bookingMsg

      -- Make sure the basket is empty
      clearBasket s
      checkBasketCount s 0

      -- Lookup the facility
      facility <- getFacilityOrExit s facilityName'

      -- Lookup the activityType
      activityType <- getActivityTypeOrExit s facility activityTypeName'

      -- Lookup the activity
      activity <- getActivityOrExit s facility activityType activityName'

      -- Get the timetableEntry
      timetableEntry
        <- getTimetableEntryOrExit s facility activityType activity date' startTime'

      -- Book the timetable slot
      bookSlot s (timetableEntry ^. timetableEntryId)

      checkBasketCount s 1

      basketItems <- getBasket s

      case basketItems ^? ix 0 of
        Just basketItem -> do
          $(logInfo) $ "Allocating credit for " <> T.pack (show basketItem)
          allocateBookingCredit s basketItem

          $(logInfo) "Submitting the basket"
          pay s

        Nothing         -> exitWithError $ "Basket empty" <> show basketItems

      checkBasketCount s 0

      $(logInfo) $ "Sucessfully booked " <> bookingMsg

getFacilityOrExit :: MonadIOLog m => S.Session -> Text -> m Facility
getFacilityOrExit s toFind =
  findFacilityByName s toFind >>=
    \case
      Nothing       -> exitWithError $ "Failed to find facility: " <> toFind
      Just facility -> logAndReturn facility
  where logAndReturn facility = do
          $(logInfo) $ "Found facility: name=" <>
                       toFind                  <>
                       ", id="                 <>
                       (T.pack . show $ facility ^. facilityId)
          return facility

getActivityTypeOrExit :: MonadIOLog m
                  => S.Session
                  -> Facility
                  -> T.Text
                  -> m ActivityType
getActivityTypeOrExit s facility toFind =
  getActivityTypeByName s (facility ^. facilityId) toFind >>=
    \case
      Nothing       -> exitWithError $ "Failed to find activity type: " <> toFind
      Just activity -> logAndReturn activity
  where logAndReturn activityType = do
          $(logInfo) $ "Found activity type: name=" <>
                       toFind                  <>
                       ", id="                 <>
                       (T.pack . show $ activityType ^. activityTypeId)
          return activityType

getActivityOrExit :: MonadIOLog m
                  => S.Session
                  -> Facility
                  -> ActivityType
                  -> T.Text
                  -> m Activity
getActivityOrExit s facility activityType toFind =
  getActivityByName s (facility ^. facilityId)
                      (activityType ^. activityTypeId)
                      (activityType ^. activityTypeBookingType)
                      toFind >>=
    \case
      Nothing       -> exitWithError $ "Failed to find activity: " <> toFind
      Just activity -> logAndReturn activity
  where logAndReturn activity = do
          $(logInfo) $ "Found activity: name=" <>
                       toFind                  <>
                       ", id="                 <>
                       (T.pack . show $ activity ^. activityId)
          return activity

getTimetableEntryOrExit :: MonadIOLog m
                        => S.Session
                        -> Facility
                        -> ActivityType
                        -> Activity
                        -> Day
                        -> Text
                        -> m TimetableEntry
getTimetableEntryOrExit s
                        facility'
                        activityType'
                        activity'
                        date'
                        startTime' =
  getTimetableEntry s (facility'     ^. facilityId    )
                      (activityType' ^. activityTypeBookingType)
                      (activity'     ^. activityId    )
                      date'
                      startTime' >>=
    \case
      Nothing -> exitWithError $ "Failed to find timetableEntry: " <>
                                  T.pack (show date')              <>
                                  " "                              <>
                                  startTime'
      Just  e -> logAndReturn e
  where logAndReturn e = do
          $(logInfo) $ "Found timetableEntry: date=" <>
                       T.pack (show date')           <>
                       ", startTime="                <>
                       startTime'                    <>
                       ", id="                       <>
                       (T.pack . show $ e ^. timetableEntryId)
          return e

checkBasketCount :: (MonadThrow m, MonadIO m) => S.Session -> Int -> m ()
checkBasketCount s expected = do
  basketItems <- getBasket s
  let count = length basketItems
  when (count /= expected) $
    exitWithError $ "Basket count not equal to expected: current count=" <>
                    show count                                           <>
                    ", expected="                                        <>
                    show expected                                        <>
                    ", basketItems="                                     <>
                    show basketItems

exitWithError :: MonadIO m => Show a => a -> m b
exitWithError msg = liftIO $ do
  print msg
  exitFailure
