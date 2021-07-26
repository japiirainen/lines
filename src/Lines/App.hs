module Lines.App
    ( App(..)
    , bootstrapApp
    )
    where

import           Lines.Prelude

import           Lines.App.Class
import           Lines.App.Error
import           Lines.Logger
import           Lines.Options
import           RIO.Directory   as Directory
import           System.Exit     as Exit
import           System.Process  as Process

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasOptions App where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasSystem App where
    getCurrentDirectory  = do
        logDebug "getCurrentDirectory"
        appIO SystemError $ Directory.getCurrentDirectory

    setCurrentDirectory path = do
        logDebug $ "setCurrentDirectory: " <> displayShow path
        appIO SystemError $ Directory.setCurrentDirectory path

    doesFileExist path = do
        logDebug $ "doesFileExist: " <> displayShow path
        appIO SystemError $ Directory.doesFileExist path

    doesDirectoryExist path = do
        logDebug $ "doesDirectoryExist: " <> displayShow path
        appIO SystemError $ Directory.doesDirectoryExist path

    listDirectory path = do
        logDebug $ "listDirectory" <> displayShow path
        appIO SystemError $ Directory.listDirectory path

    readFile path = do
        logDebug $ "readFile: " <> displayShow path
        appIO SystemError $ readFileUtf8 path

    readFileBS path = do
        logDebug $ "readFileBS: " <> displayShow path
        appIO SystemError $ readFileBinary path

    writeFile path content = do
        logDebug $ "writeFile: " <> displayShow path
        appIO SystemError $ writeFileUtf8 path content

instance HasProcess App where
    callProcess cmd args = do
        logDebug $ "call: " <> fromString cmd <> " " <> displayShow args
        appIO SystemError $ Process.callProcess cmd args

    callProcessExitCode cmd args = do
        logDebug $ "call: " <> fromString cmd <> " " <> displayShow args
        ec <- appIO SystemError
                $ Process.withCreateProcess proc
                $ \_ _ _ p -> Process.waitForProcess p
        ec <$ logDebug  ("exit code: " <> displayShow ec)
     where
        proc = (Process.proc cmd args) { Process.delegate_ctlc = True }

    readProcess cmd args stdin' = do
        logDebug $ "call: " <> fromString cmd <> " " <> displayShow args
        output <- appIO SystemError $ Process.readProcess cmd args stdin'
        output <$ logDebug ("exit code: " <> fromString output)

instance HasExit App where
    exitSuccess = do
        logDebug "exitSuccess"
        appIO SystemError Exit.exitSuccess

appIO :: MonadUnliftIO m => (IOException -> AppError) -> IO a -> m a
appIO f = mapAppError f . liftIO

data App =  App
    { appLogFunc :: LogFunc
    , appOptions :: Options
    }

bootstrapApp :: MonadIO m => Options -> m App
bootstrapApp options = runRIO app $ pure app
    where
        app = App
            { appLogFunc = linesLogFunc options
            , appOptions = options
            }
