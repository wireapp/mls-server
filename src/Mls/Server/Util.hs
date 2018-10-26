-- | Utility functions, not necessarily MLS-related.

module Mls.Server.Util
    (
    -- * Options and configs
      getOptions
    , parseConfigPath
    ) where

import Imports
import System.IO (hPutStrLn)
import System.Exit (die)
import Data.Yaml hiding (Parser)
import Options.Applicative

-- copied from types-common
getOptions
    :: FromJSON a
    => String            -- ^ Program description
    -> Maybe (Parser a)  -- ^ CLI parser for the options (if there is no config)
    -> FilePath          -- ^ Default config path, can be overridden with @--config-file@
    -> IO a
getOptions desc pars defaultPath = do
    path <- parseConfigPath defaultPath mkDesc
    file <- doesFileExist path
    case (file, pars) of
        -- Config exists, we can just take options from there
        (True, _) -> do
            configFile <- decodeFileEither path
            case configFile of
                Left e  -> fail $ show e
                Right o -> return o
        -- Config doesn't exist but at least we have a CLI options parser
        (False, Just p) -> do
            hPutStrLn stderr $
                "Config file at " ++ path ++
                " does not exist, falling back to command-line arguments. \n"
            execParser (info (helper <*> p) mkDesc)
        -- No config, no parser :(
        (False, Nothing) -> do
            die $ "Config file at " ++ path ++ " does not exist. \n"
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
