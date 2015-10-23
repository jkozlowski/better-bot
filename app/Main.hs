{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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
import           Control.Exception           (SomeException)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch         (MonadCatch, MonadThrow, catch)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger
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
                                              optionsConfigFile,
                                              dayOfWeekFromUTCTime,
                                              optsParserInfo, readConfig,
                                              _Password, _Slots)

main :: IO ()
main = execParser optsParserInfo >>= \opts -> do
  maybeConfig <- readConfig (opts ^. optionsConfigFile)
  case maybeConfig of
    Left e  -> exitWithError e
    Right c -> runStdoutLoggingT $ do
      $(logInfo) $ "Running with config: " <> (pack . Pretty.ppShow $ c)
      case opts of
        Opts (Just date) _ -> withParticularDay c date
        Opts Nothing     _ -> withCurrentTime c

withCurrentTime :: (MonadCatch m, MonadLoggerIO m)
                => [BookingConfig]
                -> m ()
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

withParticularDay :: (MonadCatch m, MonadLoggerIO m) => [BookingConfig] -> Time.Day -> m ()
withParticularDay c currentDay = do

  let dayOfWeek                = dayOfWeekFromUTCTime currentDay
  let slots                    = findSlots dayOfWeek c

  when (null slots) $ do
    $(logInfo) $ "No slots found for " <> (pack . show $ dayOfWeek)
    liftIO exitSuccess

  $(logInfo) $ "Found slots " <> (pack . Pretty.ppShow $ slots)

  forM_ slots $ \(c, slot) -> do
    let email = c ^. configEmail
    let pass  = c ^. configPassword . _Password
    s <- createSession email pass
    catch (bookActivity s c currentDay slot)
          (\(e::SomeException) -> $(logError) $ "Could not book: " <> pack (show e))

findSlots :: DayOfWeek -> [BookingConfig] -> [(Config, Text)]
findSlots day = concatMap go
  where go bc = case Map.lookup day (bc ^. bookingConfigSlots . _Slots) of
                  Just slot -> [(bc ^. bookingConfigConfig, slot)]
                  Nothing   -> []
