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

import           Control.Applicative        ((<|>))
import           Control.Exception          (Exception, bracket_)
import           Control.Exception.Lens
import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad              (forM_, unless, void, when)
import           Control.Monad.Catch        (MonadCatch, MonadThrow (throwM))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.Time                  as Time
import qualified Data.Time.Format           as Time
import           Data.Typeable
import           Network.Better.Types       (Activity (..), ActivityId (..),
                                             ActivityType (..),
                                             ActivityTypeBookingId (..),
                                             ActivityTypeId (..),
                                             BasketCount (..), BasketItem (..),
                                             Booking (..),
                                             BookingCreditStatus (..),
                                             Facility (..), FacilityId (..),
                                             ScrapingException (..),
                                             TimetableEntry (..),
                                             TimetableEntryId (..),
                                             activityName, activityTypeName,
                                             basketItemCreditStatus,
                                             basketItemRemoveUrl,
                                             bookingCreditStatusAllocateUrl,
                                             bookingCreditStatusUnallocateUrl,
                                             emptyBasketItem, facilityName,
                                             timetableEntryDate,
                                             timetableEntryStartTime,
                                             _Allocated, _ScrapingException,
                                             _Unallocated)
import qualified Network.HTTP.Client        as HTTP
import           Network.Wreq
import qualified Network.Wreq.Session       as S
import           System.IO                  (hFlush, hGetEcho, hSetEcho, stdin,
                                             stdout)
import           Text.HTML.Scalpel
import           Text.Regex.TDFA

baseURL :: String
baseURL = "https://gll.legendonlineservices.co.uk"

apiBaseURL :: String
apiBaseURL = baseURL <> "/enterprise"

mobileAPIBaseURL :: String
mobileAPIBaseURL = "https://gll.legendonlineservices.co.uk/enterprise/mobile"

createSession :: MonadIO m => T.Text -> T.Text -> m S.Session
createSession email pass = liftIO $ S.withSession $ \sess -> do
  login sess email pass
  return sess

createSessionInteractive :: MonadIO m => String -> m S.Session
createSessionInteractive email = liftIO $ S.withSession $ \sess -> do
  loginInteractive sess (T.pack email)
  return sess

loginInteractive :: MonadIO m => S.Session -> T.Text -> m ()
loginInteractive s email = getPassword >>= login s email

login :: MonadIO m => S.Session -> T.Text -> T.Text -> m ()
login s email pass = void . liftIO $ S.post s loginUrl (formParams pass)
  where loginUrl = apiBaseURL <> "/account/login"
        formParams password =
                    [ "login.IgnoreCookies"  := ("True"  :: B.ByteString)
                    , "login.HashedPassword" := ("False" :: B.ByteString)
                    , "login.Email"          := email
                    , "login.Password"       := pass
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

getActivities :: MonadIO m
              => S.Session
              -> FacilityId
              -> ActivityTypeId
              -> ActivityTypeBookingId
              -> m [Activity]
getActivities s (FacilityId facilityId)
                (ActivityTypeId activityTypeId)
                (ActivityTypeBookingId activityTypeBookingId)
  = getAsJSONWith opts s "getactivities"
    where opts = defaults & param "facilityId"     .~ paramVal facilityId
                          & param "activityTypeId" .~ paramVal activityTypeId
                          & param "bookingType"    .~ paramVal activityTypeBookingId

getActivityByName :: MonadIO m
                  => S.Session
                  -> FacilityId
                  -> ActivityTypeId
                  -> ActivityTypeBookingId
                  -> T.Text
                  -> m (Maybe Activity)
getActivityByName s facility activity activityBookingId activityName' = do
  activities <- getActivities s facility activity activityBookingId
  return $! find (\a -> a ^. activityName == activityName') activities

getTimetable :: MonadIO m
             => S.Session
             -> FacilityId
             -> ActivityTypeBookingId
             -> ActivityId
             -> m [TimetableEntry]
getTimetable s (FacilityId     facilityId)
               (ActivityTypeBookingId activityTypeBookingId)
               (ActivityId     activityId)
 = getAsJSONWith opts s "gettimetable"
 where opts = defaults & param "facilityId"     .~ paramVal facilityId
                       & param "bookingType"    .~ paramVal activityTypeBookingId
                       & param "activityId"     .~ paramVal activityId

getTimetableEntry :: MonadIO m
                  => S.Session
                  -> FacilityId
                  -> ActivityTypeBookingId
                  -> ActivityId
                  -> Time.Day
                  -> T.Text
                  -> m (Maybe TimetableEntry)
getTimetableEntry s
                  facility' activityType' activity'
                  date' startTime' = do
  timetableEntries <- getTimetable s facility' activityType' activity'
  -- e.g. Mon, 19 Oct
  let dateFormat = "%a, %d %b"
      formattedDate = T.pack $ Time.formatTime Time.defaultTimeLocale dateFormat date'
  return $! find (\e -> e ^. timetableEntryDate      == formattedDate &&
                        e ^. timetableEntryStartTime == startTime')
                 timetableEntries

bookSlot :: MonadIO m
         => S.Session
         -> TimetableEntryId
         -> m (Response B.ByteString)
bookSlot s (TimetableEntryId timetableEntryId)
 = liftIO $ S.getWith opts s (apiBaseURL <> "/bookingscentre/addsportshallbooking")
 where opts = defaults & param "slotId" .~ paramVal timetableEntryId

getBasket :: (MonadThrow m, MonadIO m) => S.Session -> m [BasketItem]
getBasket s = do
  let url = apiBaseURL <> "/basket"
  r <- liftIO $ S.getWith defaults s url
  case scrapeStringLike (r ^. responseBody) basketScraper of
    Just basketItems -> return $! basketItems
    Nothing          -> throwingM _ScrapingException (ScrapingException r)

basketScraper :: Scraper B.ByteString [BasketItem]
basketScraper = chroots divBasketItem basketItem
  where basketItem :: Scraper B.ByteString BasketItem
        basketItem = do
          creditStatus  <- unallocatedItem <|> allocatedItem
          removeBooking <- attr href removeBookingUrl
          return $! emptyBasketItem & basketItemCreditStatus .~ creditStatus
                                    & basketItemRemoveUrl .~ decodeUtf8 removeBooking

        href = "href"
        s (s' :: String) = s'
        toRegex = makeRegexOpts defaultCompOpt { multiline = False } defaultExecOpt
        decodeUtf8 = TL.toStrict . TL.decodeUtf8

        unallocatedItem = prismSelector _Unallocated allocateBookingUrl
        allocatedItem = prismSelector _Allocated unallocateBookingUrl
        prismSelector prism selector = ((prism #) . decodeUtf8) <$> attr href selector

        divBasketItem = s "div" @: [hasClass "basketItem"]

        allocateBookingUrl = regexSelector "/enterprise/Basket/AllocateBookingCredit\\?bookingid=\\d*"
        unallocateBookingUrl = regexSelector "/enterprise/Basket/UnAllocateBookingCredit\\?bookingid=.*"
        removeBookingUrl = regexSelector "/enterprise/Basket/RemoveBooking\\?bookingId=\\d*"

        regexSelector regex = s "a" @: [href @=~ toRegex (regex :: String)]

allocateBookingCredit :: (MonadIO m) => S.Session -> BasketItem -> m ()
allocateBookingCredit s basketItem = void $
  case basketItem ^? basketItemCreditStatus . _Unallocated of
    Just allocateUrl -> void $ do
      let url = baseURL <> T.unpack allocateUrl
      liftIO $ S.getWith defaults s url
    Nothing          -> liftIO . putStrLn $ "Already allocated"

pay :: (MonadIO m) => S.Session -> m ()
pay s = void . liftIO $ S.getWith defaults s (apiBaseURL <> "/Basket/Pay")

clearBasket :: (MonadThrow m, MonadIO m) => S.Session -> m ()
clearBasket s = do
  basketItems <- getBasket s
  forM_ basketItems $ \basketItem -> do
    let url = baseURL <> T.unpack (basketItem ^. basketItemRemoveUrl)
    liftIO $ S.getWith defaults s url

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
         S.getWith opts s (mobileAPIBaseURL <> "/" <> apiCall)
   return $! r ^. responseBody
 where opts = options & header "Accept" .~ ["application/json"]

paramVal :: Show a => a -> [T.Text]
paramVal val = [T.pack . show $ val]

getPassword :: MonadIO m => m T.Text
getPassword = liftIO $ do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return $! T.pack pass

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
