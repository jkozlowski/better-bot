module Network.Better.Aeson (
   jsonOptions
 , decapitalizeJsonOptions
 , jsonOptionsRemovePrefix
 ) where

import Data.Aeson.TH ( deriveJSON, defaultOptions, fieldLabelModifier )
import Data.Char     ( isUpper, toUpper, toLower )

jsonOptions = defaultOptions { fieldLabelModifier = removePrefix }

jsonOptionsRemovePrefix prefix = defaultOptions { fieldLabelModifier = (removePrefixMatch prefix) }

decapitalizeJsonOptions
  = defaultOptions { fieldLabelModifier = decapitalize . removePrefix }

removePrefix :: String -> String
removePrefix = dropWhile (not . isUpper)

removePrefixMatch :: String -> String -> String
removePrefixMatch prefix s = drop (length prefix) s

decapitalize :: String -> String
decapitalize (s:ss) = (toLower s) : ss
