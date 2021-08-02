module Lines.Options
    ( Options(..)
    , HasOptions(..)
    , DefaultOptions(..)
    , HasDefaultOptions(..)
    , parseOptions
    ) where

import qualified Env
import           Lines.Prelude
import           Options.Applicative
import qualified RIO.Text            as T
import           System.Console.ANSI (hSupportsANSI)

data ColorOption
    = AlwaysColor
    | NeverColor
    | AutoColor

data CLIOptions =
    CLIOptions { coColor           :: ColorOption
               , coTargetDirectory :: Maybe FilePath
               , coRemoteRepo      :: Maybe Text
               , coIgnoreFolders   :: Maybe Text
               }

newtype EnvOptions =
    EnvOptions { eoLogLevel :: LogLevel
               }

data Options =
    Options { oRemoteRepo      :: Maybe Text
            , oTargetDirectory :: Maybe FilePath
            , oLogLevel        :: LogLevel
            , oLogColor        :: Bool
            , oIgnoreFolders   :: [Text]
            } deriving stock (Show)

newtype DefaultOptions =
    DefaultOptions { defaultCloneDir :: FilePath
                   } deriving stock Show

class HasOptions env where
    optionsL :: Lens' env Options

class HasDefaultOptions env where
    defaultOptionsL :: Lens' env DefaultOptions

parseOptions :: IO Options
parseOptions = do
    EnvOptions {..} <- Env.parse id envParser
    CLIOptions {..} <-
        execParser $ info (optionsParser <**> helper) $ fullDesc <> progDesc
            "Lines CLI"

    logColor <- case coColor of
        AlwaysColor -> pure True
        NeverColor  -> pure False
        AutoColor   -> and <$> traverse hSupportsANSI [stdout, stderr]

    ignoreFolders <- case coIgnoreFolders of
        Nothing -> pure []
        Just t  -> pure $ T.split (== ',') t

    pure Options
        { oRemoteRepo = coRemoteRepo
        , oTargetDirectory = coTargetDirectory
        , oLogLevel = eoLogLevel
        , oLogColor = logColor
        , oIgnoreFolders = ignoreFolders
        }


envParser :: Env.Parser Env.Error EnvOptions
envParser = EnvOptions
    <$> Env.flag LevelInfo LevelDebug "DEBUG" Env.keep

optionsParser :: Parser CLIOptions
optionsParser = CLIOptions
    <$> option (eitherReader parseColorOption)
                (   long "color"
                <>  metavar "always|never|auto"
                <>  help "Colorize log messages"
                <>  value AutoColor
                )
    <*> optional (strOption
                 (   long "dir"
                 <>  short 'd'
                 <>  metavar "DIR"
                 <>  help "Target directory on local machine"
                 ))
    <*> optional (strOption
                 (   long "repo"
                 <>  short 'r'
                 <>  metavar "REPO"
                 <>  help "Remote repository URL"
                 ))
    <*> optional (strOption
                 (   long "ignore"
                 <>  short 'i'
                 <>  metavar "IGNORE"
                 <>  help "Ignore folders."
                 ))

parseColorOption :: String -> Either String ColorOption
parseColorOption = \case
    "always" -> Right AlwaysColor
    "never"  -> Right NeverColor
    "auto"   -> Right AutoColor
    x        -> Left $ "Invalid color option: " <> x
