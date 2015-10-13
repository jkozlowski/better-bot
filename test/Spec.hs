{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Aeson
import Data.Monoid ( mconcat, (<>) )
import Data.Text.IO          as T
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Network.Better.Types

main :: IO ()
main = defaultMain tests


-- html <- T.readFile "test/basket.html"
-- T.putStrLn html

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests" $ mconcat
  [ jsonTests "Booking"        bookingJSON            bookingValue
  , jsonTests "Facility"       facilityJSON           facilityValue
  , jsonTests "BasketCount"    basketCountJSON        basketCountValue
  , jsonTests "ActivityType"   activityTypeJSON       activityTypeValue
  , jsonTests "Activity"       activityJSON           activityValue
  , jsonTests "TimetableEntry" timetableEntryJSON     timetableEntryValue
  ]

jsonTests typeName json value =
  [ testCase (typeName <> " ToJSON") $
      encode value @?= json
  , testCase (typeName <> " FromJSON") $
      decode json  @?= Just value
  ]

bookingJSON = mconcat
  [ "{\"ParentId\":0"
  , ",\"ParentTitle\":null"
  , ",\"Name\":\"Jakub Kozlowsk - Squash 40 Minutes\""
  , ",\"Id\":12893893"
  , ",\"RemainingSlots\":0"
  , ",\"Instructor\":\"\""
  , ",\"Location\":\"Squash Court 40 mins 3\""
  , ",\"Facility\":\"Oasis\""
  , ",\"FacilityId\":197"
  , ",\"Date\":\"Sun, 27 Sep\""
  , ",\"StartTime\":\"16:00\""
  , ",\"EndTime\":\"16:40\""
  , ",\"CanCancel\":false"
  , ",\"Status\":\"Attended\""
  --, ",\"ExcerciseCategory\":null,"
  , ",\"CanSelectCourt\":false"
  --, ",\"Description\":null"
  --, ",\"Items\":[]"
  , "}"
  ]

-- Booking

bookingValue
 = emptyBooking & bookingParentId       .~ 0
                & bookingParentTitle    .~ Nothing
                & bookingName           .~ "Jakub Kozlowsk - Squash 40 Minutes"
                & bookingId             .~ 12893893
                & bookingRemainingSlots .~ 0
                & bookingInstructor     .~ ""
                & bookingLocation       .~ "Squash Court 40 mins 3"
                & bookingFacility       .~ "Oasis"
                & bookingFacilityId     .~ 197
                & bookingDate           .~ "Sun, 27 Sep"
                & bookingStartTime      .~ "16:00"
                & bookingEndTime        .~ "16:40"
                & bookingCanCancel      .~ False
                & bookingStatus         .~ "Attended"
                & bookingCanSelectCourt .~ False

-- Facility

facilityJSON = mconcat
  [ "{\"Name\":\"Oasis\""
  , ",\"Id\":197"
  , ",\"IsHomeClub\":true"
  , ",\"Address1\":null"
  , ",\"Address2\":null"
  , ",\"Address3\":null"
  , ",\"City\":null"
  , ",\"PostCode\":null"
  , ",\"Phone\":null"
  , ",\"LocationId\":6071"
  , "}"
  ]

facilityValue
  = emptyFacility & facilityName       .~ "Oasis"
                  & facilityId         .~ FacilityId 197
                  & facilityIsHomeClub .~ True
                  & facilityLocationId .~ 6071

-- BasketCount

basketCountJSON = "{\"basketCount\":0}"

basketCountValue
  = emptyBasketCount & basketBasketCount .~ 0

-- ActivityType
activityTypeJSON = mconcat
  [ "{\"Name\":\"Group Exercise Classes\""
  , ",\"Id\":1"
  , ",\"FacilityId\":197"
  , "}"
  ]

activityTypeValue
  = emptyActivityType & activityTypeName       .~ "Group Exercise Classes"
                      & activityTypeId         .~ ActivityTypeId 1
                      & activityTypeFacilityId .~ FacilityId 197

-- Activity
activityJSON = mconcat
  [ "{\"Name\":\"Squash\""
  , ",\"Id\":444"
  , ",\"ActivityTypeId\":3"
  , "}"
  ]

activityValue
  = emptyActivity & activityName           .~ "Squash"
                  & activityId             .~ ActivityId 444
                  & activityActivityTypeId .~ ActivityTypeId 3

-- TimetableEntry
timetableEntryJSON = mconcat
  [ "{\"ParentId\":444"
  -- , ",\"ParentTitle\":null"
  -- , ",\"Name\":\"Squash\""
  , ",\"Id\":6909"
  , ",\"RemainingSlots\":2"
  -- , ",\"Instructor\":null"
  -- , ",\"Location\":\"Oasis\""
  -- , ",\"Facility\":\"Oasis\""
  -- , ",\"FacilityId\":197"
  , ",\"Date\":\"Sun, 04 Oct\""
  , ",\"StartTime\":\"16:00\""
  , ",\"EndTime\":\"16:40\""
  -- , ",\"CanCancel\":false"
  -- , ",\"Status\":null"
  -- , ",\"ExcerciseCategory\":null"
  -- , ",\"CanSelectCourt\":true"
  -- , ",\"Description\":null"
  -- , ",\"Items\":[]"
  , "}"
  ]

timetableEntryValue
  = emptyTimetableEntry & timetableEntryParentId       .~ 444
                        & timetableEntryId             .~ TimetableEntryId 6909
                        & timetableEntryRemainingSlots .~ 2
                        & timetableEntryDate           .~ "Sun, 04 Oct"
                        & timetableEntryStartTime      .~ "16:00"
                        & timetableEntryEndTime        .~ "16:40"

-- {"Name":"Squash","Id":6963,"Instructor":null,"Location":"Oasis","Price":12.95,"Discount":-3.35,"Total":9.60,"Date":"Wed, 07 Oct","StartTime":"11:20","EndTime":"12:00","RemainingSlots":3,"FacilityId":197,"Facility":"Oasis","ErrorMessage":null,"NoBookReason":"available","ActivityId":444,"FullDate":"2015-10-07","Description":null,"CanBook":true}
