{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
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

import           Control.Lens.TH             (makeClassy, makePrisms)
import           Data.Aeson
import qualified Data.Aeson.TH               as A
import qualified Data.ByteString             as B
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (maybe)
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as T
import qualified Data.Time                   as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Data.Time.Clock             as Time
import qualified Data.Time.Format            as Time
import qualified Data.Yaml                   as Yaml
import           Network.Better.Aeson        (decapitalizeJsonOptionsRemovePrefix)
import           Options.Applicative
import           Options.Applicative.Builder
import           System.IO                   (stdin)

data Opts = Opts
  { _optionsOverrideDate :: Maybe Time.Day
  , _optionsConfigFile   :: Maybe FilePath
  } deriving (Show, Eq)

$(makeClassy ''Opts)
$(makePrisms ''Opts)

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (helper <*> optsParser)
      ( fullDesc
     <> progDesc "Book activity slots as per config"
     <> header "better-bot - books activity slots" )

optsParser :: Parser Opts
optsParser = Opts
  <$> option (Just <$> dateReader)
      ( long "date"
     <> value Nothing
     <> metavar "DATE"
     <> help ("Date in yyyy-mm-dd format. If specified, "                <>
              "this date will be used for the bookings and appropriate " <>
              " slot will be looked up in the config (by figuring out "  <>
              " what day of the week this particular date falls on)."))
  <*> optional (strOption
       (  long "config"
       <> metavar "FILENAME"
       <> help ("Optional path to the config file. If not specified " <>
                "config will be read from stdin. ")))

  where dateReader :: ReadM Time.Day
        dateReader = do
          dateValue <- str
          -- yyyy-mm-dd
          let dateFormat = "%F"
          let formatted = Time.parseTimeM True Time.defaultTimeLocale dateFormat dateValue
          maybe (readerError $ "Could not parse date: " <> dateValue)
                pure
                formatted

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

dayOfWeekFromUTCTime :: Time.Day -> DayOfWeek
dayOfWeekFromUTCTime day =
  let (_, _, weekDay) = Time.toWeekDate day
  in toEnum (weekDay - 1) -- Enums are numbered [0,n];
                          -- toWeekDate returns [1,7]

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

newtype Password = Password Text
deriving instance (FromJSON Password)
deriving instance (ToJSON Password)
deriving instance (Eq Password)
instance Show Password where
  show (Password _) = "\"*******\""

$(makeClassy ''Password)
$(makePrisms ''Password)

data Config = Config
  { _configFacility     :: {-# UNPACK #-} !Text
  , _configActivityType :: {-# UNPACK #-} !Text
  , _configActivity     :: {-# UNPACK #-} !Text
  , _configEmail        :: {-# UNPACK #-} !Text
  , _configPassword     :: {-# UNPACK #-} !Password
  } deriving (Eq, Show)

$(makeClassy ''Config)
$(makePrisms ''Config)
$(A.deriveJSON (decapitalizeJsonOptionsRemovePrefix "_config") ''Config)

data BookingConfig = BookingConfig
  { _bookingConfigConfig :: !Config
  , _bookingConfigSlots  :: !Slots
  } deriving (Eq, Show)

$(makeClassy ''BookingConfig)
$(makePrisms ''BookingConfig)
$(A.deriveJSON (decapitalizeJsonOptionsRemovePrefix "_bookingConfig") ''BookingConfig)

-- TODO: Check that there are no duplicate sections in the config
readConfig :: Maybe FilePath -> IO (Either Yaml.ParseException [BookingConfig])
readConfig maybeConfig =
  let config = case maybeConfig of
                  Just fileName -> B.readFile fileName
                  Nothing       -> B.hGetContents stdin
  in Yaml.decodeEither' <$> config 
