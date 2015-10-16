{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Better.Types
-- Copyright   :  (c) 2015, Jakub Dominik Kozlowski
-- License     :  MIT
--
-- Maintainer  :  mail@jakub-kozlowski.com
--
-- Contains API definition for interacting with Better API.
-----------------------------------------------------------------------------
module Network.Better.Session where


import           Control.Exception       ( bracket_ )
import           Control.Lens
import           Control.Monad           ( void )
import           Control.Monad.IO.Class  ( MonadIO, liftIO )
import           Data.List               ( find )
import           Data.Monoid             ( (<>) )
import           Network.Better.Types    ( Booking(..)
                                         , Facility(..), facilityName
                                         , FacilityId(..) , BasketCount(..)
                                         , ActivityTypeId(..)
                                         , ActivityType(..), activityTypeName
                                         , ActivityId(..)
                                         , Activity(..), activityName
                                         , TimetableEntryId(..)
                                         , TimetableEntry(..)
                                         , timetableEntryDate
                                         , timetableEntryStartTime
                                         , BasketItemId(..)
                                         , BasketItem(..)
                                         , emptyBasketItem
                                         , basketItemId
                                         , basketItemAllocateBookingCreditUrl )
import           Network.Wreq
import           System.IO               ( stdout, stdin, hGetEcho, hFlush
                                         , hSetEcho )
import           Text.HTML.Scalpel
import           Text.Regex.TDFA
import qualified Debug.Trace                as Debug
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Network.Wreq.Session       as S


import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Client as HTTP

baseURL :: String
baseURL = "https://gll.legendonlineservices.co.uk/enterprise"

apiBaseURL :: String
apiBaseURL = "https://gll.legendonlineservices.co.uk/enterprise/mobile"

createSession :: MonadIO m => m S.Session
createSession = liftIO $ S.withSession $ \sess -> do
  login sess "mail@jakub-kozlowski.com"
  return sess

login :: MonadIO m => S.Session -> String -> m ()
login s email = do
  password <- getPassword
  void . liftIO $ S.post s loginUrl (formParams password)
 where loginUrl = baseURL <> "/account/login"
       formParams password =
                    [ "login.IgnoreCookies"  := ("True"  :: B.ByteString)
                    , "login.HashedPassword" := ("False" :: B.ByteString)
                    , "login.Email"          := email
                    , "login.Password"       := password
                    , "login.Remember"       := ("false" :: B.ByteString)
                    ]

-- |
-- Better API

getUserBookings :: MonadIO m => S.Session -> m [Booking]
getUserBookings s = getAsJSON s "getuserbookings"

getFacilities :: MonadIO m => S.Session -> m [Facility]
getFacilities s =  getAsJSON s "getfacilities"

findFacilityByName :: MonadIO m =>  S.Session -> T.Text -> m (Maybe Facility)
findFacilityByName s facility = do
  facilities <- getFacilities s
  return $! find (\f -> f ^. facilityName == facility) facilities

getBasketCount :: MonadIO m => S.Session -> m BasketCount
getBasketCount s = getAsJSON s "getbasketcount"

getActivityTypes :: MonadIO m => S.Session -> FacilityId -> m [ActivityType]
getActivityTypes s (FacilityId facilityId)
  = getAsJSONWith (defaults & param "facilityId" .~ [T.pack . show $ facilityId])
                  s
                  "getactivitytypes"

getActivityTypeByName :: MonadIO m
                      => S.Session
                      -> FacilityId
                      -> T.Text
                      -> m (Maybe ActivityType)
getActivityTypeByName s facility activity = do
  activityTypes <- getActivityTypes s facility
  return $! find (\a -> a ^. activityTypeName == activity) activityTypes

getActivities :: MonadIO m => S.Session -> FacilityId -> ActivityTypeId -> m [Activity]
getActivities s (FacilityId facilityId) (ActivityTypeId activityTypeId)
  = getAsJSONWith opts s "getactivities"
    where opts = defaults & param "facilityId"     .~ paramVal facilityId
                          & param "activityTypeId" .~ paramVal activityTypeId

getActivityByName :: MonadIO m
                  => S.Session
                  -> FacilityId
                  -> ActivityTypeId
                  -> T.Text
                  -> m (Maybe Activity)
getActivityByName s facility activity activityName' = do
  activities <- getActivities s facility activity
  return $! find (\a -> a ^. activityName == activityName') activities

getTimetable :: MonadIO m
             => S.Session
             -> FacilityId
             -> ActivityTypeId
             -> ActivityId
             -> m [TimetableEntry]
getTimetable s (FacilityId     facilityId)
               (ActivityTypeId activityTypeId)
               (ActivityId     activityId)
 = getAsJSONWith opts s "gettimetable"
 where opts = defaults & param "facilityId"     .~ paramVal facilityId
                       & param "activityTypeId" .~ paramVal activityTypeId
                       & param "activityId"     .~ paramVal activityId

getTimetableEntry :: MonadIO m
                  => S.Session
                  -> FacilityId
                  -> ActivityTypeId
                  -> ActivityId
                  -> T.Text
                  -> T.Text
                  -> m (Maybe TimetableEntry)
getTimetableEntry s
                  facility' activityType' activity'
                  date' startTime' = do
  timetableEntries <- getTimetable s facility' activityType' activity'
  return $! find (\e -> e ^. timetableEntryDate      == date'    &&
                        e ^. timetableEntryStartTime == startTime')
                 timetableEntries

bookSlot :: MonadIO m
         => S.Session
         -> TimetableEntryId
         -> m (Response B.ByteString)
bookSlot s (TimetableEntryId timetableEntryId)
 = liftIO $ S.getWith opts s "https://gll.legendonlineservices.co.uk/enterprise/bookingscentre/addsportshallbooking"
 where opts = defaults & param "slotId" .~ paramVal timetableEntryId

getBasket :: MonadIO m => S.Session -> m (Maybe [BasketItem])
getBasket s = do
  r <- liftIO $ S.getWith defaults s ("https://gll.legendonlineservices.co.uk/enterprise/basket")
  return $! scrapeStringLike (r ^. responseBody) basketScraper

basketScraper :: Scraper B.ByteString [BasketItem]
basketScraper = chroots divBasketItem basketItem
  where basketItem :: Scraper B.ByteString BasketItem
        basketItem = do
          allocateBooking <- attr "href" aHref
          return $! emptyBasketItem & basketItemId     .~ BasketItemId 1
                                    & basketItemAllocateBookingCreditUrl .~ (TL.toStrict . TL.decodeUtf8 $ allocateBooking)

        toRegex = makeRegexOpts defaultCompOpt { multiline = False } defaultExecOpt

        divBasketItem :: Selector
        divBasketItem = (("div" :: String) @: [hasClass "basketItem"])

        aHref :: Selector
        aHref = ("a" :: String) @: [("href" :: String) @=~ (toRegex ("/enterprise/Basket/AllocateBookingCredit.*" :: String))]

-- |
-- Helper functions

getAsJSON :: (MonadIO m, Aeson.FromJSON a) => S.Session -> String -> m a
getAsJSON = getAsJSONWith defaults

getAsJSONWith :: (MonadIO m, Aeson.FromJSON a)
              => Options
              -> S.Session
              -> String
              -> m a
getAsJSONWith options s apiCall = liftIO $ do
   r <- asJSON1 =<<
         S.getWith opts s (apiBaseURL <> "/" <> apiCall)
   return $! r ^. responseBody
 where opts = options & header "Accept" .~ ["application/json"]

paramVal :: Show a => a -> [T.Text]
paramVal val = [T.pack . show $ val]

getPassword :: MonadIO m => m String
getPassword = liftIO $ do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

-- |
-- Copied functions
asJSON1 resp = do
  let contentType = fst . BS.break (==59) . fromMaybe "unknown" .
                    lookup "Content-Type" . HTTP.responseHeaders $ resp
  unless ("application/json" `BS.isPrefixOf` contentType) $
    throwM . JSONError $ "content type of response is " ++ show contentType
  case Aeson.eitherDecode' (HTTP.responseBody resp) of
    Left err  -> throwM (JSONError err)
    Right val -> return (fmap (const val) resp)
