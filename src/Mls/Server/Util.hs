-- | Utility functions, not necessarily MLS-related.

module Mls.Server.Util
    (
    -- * Options and configs
      getOptions
    , parseConfigPath
    ) where

import Imports
import System.Exit (die)
import Data.Yaml hiding (Parser)
import Options.Applicative

-- copied from types-common and modified to allow env options as well
getOptions
    :: FromJSON a
    => String            -- ^ Program description
    -> FilePath          -- ^ Default config path, can be overridden with
                         --   @--config-file@ or with @CONFIG_FILE@
    -> IO a
getOptions desc defaultPath = do
    -- We're using the fact that the 'Alternative' instance for IO picks the
    -- first thing that doesn't throw an IO error.
    path <- getEnv "CONFIG_FILE"
        <|> parseConfigPath defaultPath mkDesc
    exists <- doesFileExist path
    unless exists $
        die $ "Config file at " ++ path ++ " does not exist. \n"
    configFile <- decodeFileEither path
    case configFile of
        Left e  -> fail $ show e
        Right o -> return o
  where
    mkDesc = header desc <> fullDesc

-- copied from types-common
parseConfigPath :: FilePath -> InfoMod String -> IO String
parseConfigPath defaultPath desc = do
    args <- getArgs
    let result =
            getParseResult $
            execParserPure defaultPrefs (info (helper <*> pathParser) desc) args
    pure $ fromMaybe defaultPath result
  where
    pathParser :: Parser String
    pathParser =
        strOption $
        long "config-file" <> short 'c' <> help "Config file to load" <>
        showDefault <>
        value defaultPath
