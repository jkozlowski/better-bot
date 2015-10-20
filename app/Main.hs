{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
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

import           BookSlot
import           Control.Concurrent.Async    (async, mapConcurrently)
import           Control.Exception           (SomeException)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow,
                                              catch)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Control.Retry               (constantDelay,
                                              limitRetriesByDelay, recoverAll)
import           Data.List                   (concatMap)
import           Data.List.Lens
import qualified Data.Map.Strict             as Map
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text, pack)
import qualified Data.Time.Calendar          as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Data.Time.Clock             as Time
import           Network.Better.Session
import           Network.Better.Types
import qualified Network.Wreq.Session        as S
import           Options.Applicative         (execParser)
import           System.Exit                 (exitFailure, exitSuccess)
import qualified Text.Show.Pretty            as Pretty
import           Types                       (BookingConfig, Config, DayOfWeek,
                                              Opts (..), bookingConfig,
                                              bookingConfigConfig,
                                              bookingConfigSlots, configEmail,
                                              configPassword,
                                              dayOfWeekFromUTCTime,
                                              optsParserInfo, readConfig,
                                              _Password, _Slots)

main :: IO ()
main = execParser optsParserInfo >>= \opts -> do
  maybeConfig <- readConfig
  case maybeConfig of
    Left e  -> exitWithError e
    Right c -> runStdoutLoggingT $ do
      $(logInfo) $ "Running with config: " <> (pack . Pretty.ppShow $ c)
      case opts of
        Opts (Just date) -> withParticularDay c date
        Opts Nothing     -> withCurrentTime c

withCurrentTime :: (MonadCatch m, MonadLogger m, MonadMask m, MonadIO m)
                => [BookingConfig] -> m ()
withCurrentTime c = do
  currentTime <- liftIO Time.getCurrentTime

  let currentDay               = Time.utctDay currentTime
  let sameDayNextWeek          = Time.addDays 7 currentDay
  let dayOfWeek                = dayOfWeekFromUTCTime currentDay
  let sameDayNextWeekDayOfWeek = dayOfWeekFromUTCTime sameDayNextWeek

  $(logInfo) $ "Today is "       <> (pack . Time.showGregorian $ currentDay)
  $(logInfo) $ "Next week is "   <> (pack . Time.showGregorian $ sameDayNextWeek)
  $(logInfo) $ "Day of week is " <> (pack . show $ dayOfWeek)

  when (dayOfWeek /= sameDayNextWeekDayOfWeek) $
    exitWithError ("Day of week for today ("                        <>
                     show dayOfWeek <> ") "                         <>
                   "is not the same as day of week for next week (" <>
                     show sameDayNextWeekDayOfWeek <> ")")

  withParticularDay c sameDayNextWeek

withParticularDay :: (MonadCatch m, MonadMask m, MonadLogger m, MonadIO m)
                  => [BookingConfig] -> Time.Day -> m ()
withParticularDay c currentDay = do

  let dayOfWeek                = dayOfWeekFromUTCTime currentDay
  let slots                    = findSlots dayOfWeek c

  when (null slots) $ do
    $(logInfo) $ "No slots found for " <> (pack . show $ dayOfWeek)
    liftIO exitSuccess

  $(logInfo) $ "Found slots " <> (pack . Pretty.ppShow $ slots)


  -- _ <- liftIO $ mapConcurrently (\(c, slot) -> do
  --   let email = c ^. configEmail
  --   let pass  = c ^. configPassword . _Password
  --   s <- createSession email pass
  --   liftIO $ bookActivity s c currentDay slot) slots
  --   --retry $ liftIO $

  $(logInfo) "Finished"

retry :: (MonadIO m, MonadMask m) => m a -> m a
retry = recoverAll (limitRetriesByDelay timeoutMicros $ constantDelay delayMicros)
  where delayMicros     = 1 * microsInSeconds
        timeoutMicros   = 1 * 60 * microsInSeconds
        microsInSeconds = 1000000

bookAll :: --(MonadLoggerIO m, MonadLogger IO, MonadIO m)
        -- =>
        [(Config, Text)] -> Time.Day -> IO [()]
bookAll slots currentDay = mapConcurrently (\(c, slot) -> do
  let email = c ^. configEmail
  let pass  = c ^. configPassword . _Password
  s <- createSession email pass
  bookActivity s c currentDay slot) slots

findSlots :: DayOfWeek -> [BookingConfig] -> [(Config, Text)]
findSlots day = concatMap go
  where go bc = case Map.lookup day (bc ^. bookingConfigSlots . _Slots) of
                  Just slot -> [(bc ^. bookingConfigConfig, slot)]
                  Nothing   -> []
