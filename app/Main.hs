{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ConstraintKinds   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2015, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Main for the bot.
-----------------------------------------------------------------------------
module Main where

import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Monoid            ( (<>) )
import           Data.Text              ( Text )
import           Network.Better.Session
import           Network.Better.Types
import           System.Exit            (exitFailure)
import qualified Data.Text              as T
import qualified Network.Wreq.Session   as S

main :: IO ()
main = do
  s <- createSession
  runStdoutLoggingT (bookActivity s "Oasis" "Other Activities" "Squash"
                                    "Mon, 19 Oct" "12:40")
  return ()

type MonadIOLog m = (MonadIO m, MonadLogger m)

bookActivity :: MonadIOLog m
             => S.Session
             -> Text
             -> Text
             -> Text
             -> Text
             -> Text
             -> m ()
bookActivity s
             facilityName'
             activityTypeName'
             activityName'
             date'
             startTime'
 = do basketItems <- getBasket s

      -- Make sure the basket is empty
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

      basketCount <- getBasketCount s

      checkBasketCount s 1

      basketItems <- getBasket s

      $(logInfo) $ "Basket: " <> (T.pack . show $ basketItems)

      return ()

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
                        -> Text
                        -> Text
                        -> m TimetableEntry
getTimetableEntryOrExit s
                        facility'
                        activityType'
                        activity'
                        date'
                        startTime' =
  getTimetableEntry s (facility'     ^. facilityId    )
                      (activityType' ^. activityTypeId)
                      (activity'     ^. activityId    )
                      date'
                      startTime' >>=
    \case
      Nothing -> exitWithError $ "Failed to find timetableEntry: " <>
                                  date'                            <>
                                  " "                              <>
                                  startTime'
      Just  e -> logAndReturn e
  where logAndReturn e = do
          $(logInfo) $ "Found timetableEntry: date=" <>
                       date'                         <>
                       ", startTime="                <>
                       startTime'                    <>
                       ", id="                       <>
                       (T.pack . show $ e ^. timetableEntryId)
          return e

checkBasketCount :: MonadIO m => S.Session -> Int -> m ()
checkBasketCount s expected = do
  basketCount <- getBasketCount s
  let count = basketCount ^. basketBasketCount
  if count /= expected
    then exitWithError $ "Basket count not equal to expected: current count=" <>
                         show count                                           <>
                         ", expected="                                        <>
                         show expected
    else return ()

exitWithError :: MonadIO m => Show a => a -> m b
exitWithError msg = liftIO $ do
  putStrLn $ show msg
  exitFailure
