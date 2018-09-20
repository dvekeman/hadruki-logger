{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Hadruki.Logger
    ( Verbosity (..)
    , Config (..)
    , Handle
    , withHandle
    , createHandle

    , debug
    , info
    , warning
    , error

    , debug'
    , info'
    , warning'
    , error'
    ) where

import           Control.Applicative   (Alternative (..))
import           Control.Exception     (bracket)
import           Data.Monoid
import qualified Data.Aeson            as A
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Data.Time             (getCurrentTime)
import           Data.Time.Format      (formatTime, iso8601DateFormat, defaultTimeLocale)
import           Prelude               hiding (error, log)
import qualified System.Log.FastLogger as FL

data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord)

instance Show Verbosity where
  show Debug   = "DEBUG  "
  show Info    = "INFO   "
  show Warning = "WARNING"
  show Error   = "ERROR  "

instance A.FromJSON Verbosity where
    parseJSON = A.withText "FromJSON Hadruki.Logger.Verbosity" $ \t ->
        case t of
            "debug"   -> pure Debug
            "info"    -> pure Info
            "warning" -> pure Warning
            "error"   -> pure Error
            _         -> fail $ "Unknown verbosity: " ++ T.unpack t

data Config = Config
    { cPath      :: Maybe FilePath
    , cVerbosity :: Maybe Verbosity
    } deriving (Show)

instance Monoid Config where
    mempty                              = Config (Just "-") (Just Warning)
    Config p0 v0 `mappend` Config p1 v1 = Config (p0 <|> p1) (v0 <|> v1)

instance A.FromJSON Config where
    parseJSON = A.withObject "FromJSON Hadruki.Logger.Config" $ \o -> Config
        <$> o A..:? "path"
        <*> o A..:? "verbosity"

data Handle = Handle
    { hConfig    :: Config
    , hLoggerSet :: FL.LoggerSet
    } deriving Show
instance Show FL.LoggerSet where 
  show _ = "N/A"

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = bracket
    (case cPath config of
        Nothing   -> FL.newStderrLoggerSet FL.defaultBufSize
        Just "-"  -> FL.newStderrLoggerSet FL.defaultBufSize
        Just path -> FL.newFileLoggerSet FL.defaultBufSize path)
    FL.rmLoggerSet
    (\l -> f Handle {hConfig = config, hLoggerSet = l})

createHandle :: Config -> IO Handle
createHandle config = bracket
    (case cPath config of
        Nothing   -> FL.newStderrLoggerSet FL.defaultBufSize
        Just "-"  -> FL.newStderrLoggerSet FL.defaultBufSize
        Just path -> FL.newFileLoggerSet FL.defaultBufSize path)
    FL.rmLoggerSet
    (\l -> return Handle {hConfig = config, hLoggerSet = l})


log :: FL.ToLogStr s => Handle -> Verbosity -> s -> IO ()
log Handle {..} v x
    | v >= verbosity = do
            now <- formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S.%q")) <$> getCurrentTime 
            FL.pushLogStrLn hLoggerSet $ FL.toLogStr (show v) <> " - " <> FL.toLogStr now <> " - " <> FL.toLogStr x
    | otherwise      = return ()
  where
    verbosity = fromMaybe Debug (cVerbosity hConfig)

debug, info, warning, error :: FL.ToLogStr str => Handle -> str -> IO ()
debug   h = log h Debug
info    h = log h Info
warning h = log h Warning
error   h = log h Error

debug', info', warning', error' :: Handle -> String -> IO ()
debug'   = debug
info'    = info
warning' = warning
error'   = error

