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
import           System.Console.ANSI (hSupportsANSI)

data ColorOption
    = AlwaysColor
    | NeverColor
    | AutoColor

data CLIOptions =
    CLIOptions { coColor           :: ColorOption
               , coTargetDirectory :: Maybe FilePath
               , coRemoteRepo      :: Maybe Text
               }

newtype EnvOptions =
    EnvOptions { eoLogLevel :: LogLevel
               }

data Options =
    Options { oRemoteRepo      :: Maybe Text
            , oTargetDirectory :: Maybe FilePath
            , oLogLevel        :: LogLevel
            , oLogColor        :: Bool
            } deriving stock (Show)

data DefaultOptions =
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

    pure Options
        { oRemoteRepo = coRemoteRepo
        , oTargetDirectory = coTargetDirectory
        , oLogLevel = eoLogLevel
        , oLogColor = logColor
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
                 (   long "target-directory"
                 <>  short 'd'
                 <>  metavar "DIR"
                 <>  help "Target directory on local machine"
                 ))
    <*> optional (strOption
                 (   long "remote-repository"
                 <>  short 'r'
                 <>  metavar "REPO"
                 <>  help "Github repository URL"
                 ))

parseColorOption :: String -> Either String ColorOption
parseColorOption = \case
    "always" -> Right AlwaysColor
    "never"  -> Right NeverColor
    "auto"   -> Right AutoColor
    x        -> Left $ "Invalid color option: " <> x
