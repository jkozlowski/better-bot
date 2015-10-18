module Network.Better.Aeson (
   jsonOptions
 , decapitalizeJsonOptions
 , decapitalizeJsonOptionsRemovePrefix
 , jsonOptionsRemovePrefix
 ) where

import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import Data.Char     ( isUpper, toUpper, toLower )

jsonOptions = defaultOptions { fieldLabelModifier = removePrefix }

jsonOptionsRemovePrefix prefix = defaultOptions { fieldLabelModifier = removePrefixMatch prefix }

decapitalizeJsonOptions
  = defaultOptions { fieldLabelModifier = decapitalize . removePrefix }

decapitalizeJsonOptionsRemovePrefix prefix
  = defaultOptions { fieldLabelModifier = decapitalize . removePrefixMatch prefix }

removePrefix :: String -> String
removePrefix = dropWhile (not . isUpper)

removePrefixMatch :: String -> String -> String
removePrefixMatch prefix = drop (length prefix)

decapitalize :: String -> String
decapitalize (s:ss) = toLower s : ss
