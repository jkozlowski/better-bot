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
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow,
                                              onException)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Retry               (constantDelay,
                                              limitRetries, recoverAll)
import qualified Data.ByteString.Char8       as S8
import           Data.Char                   (toLower)
import           Data.List                   (concatMap)
import           Data.List.Lens
import qualified Data.Map.Strict             as Map
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Encoding.Error    as T
import qualified Data.Text.IO                as T
import qualified Data.Time.Calendar          as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Data.Time.Clock             as Time
import qualified Data.Time.Format            as Time
import           Language.Haskell.TH.Syntax
import           Network.Better.Session
import           Network.Better.Types
import qualified Network.Wreq.Session        as S
import           Options.Applicative         (execParser)
import           System.Exit                 (exitFailure, exitSuccess)
import           System.IO                   (Handle, stdout)
import           System.Log.FastLogger       (fromLogStr)
import qualified Text.Show.Pretty            as Pretty
import           Types                       (BookingConfig, Config, DayOfWeek,
                                              Opts (..), bookingConfig,
                                              bookingConfigConfig,
                                              bookingConfigSlots, configEmail,
                                              configPassword,
                                              dayOfWeekFromUTCTime,
                                              optionsConfigFile, optsParserInfo,
                                              readConfig, _Password, _Slots)


main :: IO ()
main = execParser optsParserInfo >>= \opts -> do
  maybeConfig <- readConfig (opts ^. optionsConfigFile)
  case maybeConfig of
    Left e  -> exitWithError e
    Right c -> runAppLogging $ do
      $(logInfo) $ "Running with config: " <> (pack . Pretty.ppShow $ c)
      case opts of
        Opts (Just date) _ -> withParticularDay c date
        Opts Nothing     _ -> withCurrentTime c

withCurrentTime :: (MonadCatch m, MonadMask m, MonadLoggerIO m)
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

withParticularDay :: (MonadCatch m, MonadMask m, MonadLoggerIO m)
                  => [BookingConfig] -> Time.Day -> m ()
withParticularDay c currentDay = do

  let dayOfWeek                = dayOfWeekFromUTCTime currentDay
  let slots                    = findSlots dayOfWeek c

  when (null slots) $ do
    $(logInfo) $ "No slots found for " <> (pack . show $ dayOfWeek)
    liftIO exitSuccess

  $(logInfo) $ "Found slots " <> (pack . Pretty.ppShow $ slots)

  retry $ forM_ slots $ \(c, slot) -> do
    let email = c ^. configEmail
    let pass  = c ^. configPassword . _Password
    s <- createSession email pass
    onException (bookActivity s c currentDay slot)
                ($(logError) $ "Could not book")

findSlots :: DayOfWeek -> [BookingConfig] -> [(Config, Text)]
findSlots day = concatMap go
  where go bc = case Map.lookup day (bc ^. bookingConfigSlots . _Slots) of
                  Just slot -> [(bc ^. bookingConfigConfig, slot)]
                  Nothing   -> []

-- retries
retry :: (MonadIO m, MonadMask m) => m a -> m a
retry = recoverAll (limitRetries maxRetries <> constantDelay delayMicros)
  where delayMicros     = 2 * microsInSeconds
        maxRetries      = 15
        microsInSeconds = 1000000

-- Logging setup

runAppLogging :: MonadIO m => LoggingT m a -> m a
runAppLogging = (`runLoggingT` loggerFunc)

-- | Logging function takes the log level into account - stolen from stack.
loggerFunc :: (MonadIO m, ToLogStr msg)
           => Loc -> Text -> LogLevel -> msg -> m ()
loggerFunc loc _src level msg =
  liftIO (getOutput >>= T.hPutStrLn outputChannel)
  where outputChannel = stdout
        getOutput =
          do date <- getDate
             l <- getLevel
             lc <- getLoc
             return (T.pack date <> T.pack l <> T.decodeUtf8 (fromLogStr (toLogStr msg)) <> T.pack lc)
          where getDate =
                  do now <- Time.getCurrentTime
                     return (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %T%Q" now ++
                             ": ")
                getLevel =
                    return ("[" ++
                            map toLower (drop 5 (show level)) ++
                            "] ")
                getLoc =
                    return (" @(" ++ fileLocStr ++ ")")
                fileLocStr =
                  loc_package loc ++
                  ':' :
                  loc_module loc ++
                  ' ' :
                  loc_filename loc ++
                  ':' :
                  line loc ++
                  ':' :
                  char loc
                  where line = show . fst . loc_start
                        char = show . snd . loc_start
