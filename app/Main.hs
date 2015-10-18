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

import           Types                 (readConfig)
import           BookSlot
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Data.List.Lens
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.Better.Session
import           Network.Better.Types
import qualified Network.Wreq.Session   as S
import           System.Exit            (exitFailure)

main :: IO ()
main = do
  config <- readConfig

  -- 1) Figure out what day of the week it is.
  -- 2) Find slots to book
  -- 3) Book'em

  print config
  -- s <- createSession "mail@jakub-kozlowski.com"
  -- catch (runStdoutLoggingT (bookActivity s "Oasis" "Other Activities" "Squash"
  --                                          "Thu, 22 Oct" "16:00"))
  --       (\(e::SomeException) -> runStdoutLoggingT($(logError) $ "Could not book: " <> T.pack (show e)))
  return ()
