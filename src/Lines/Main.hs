module Lines.Main
    (linesMain
    )
    where

import           Lines.App.Class
import           Lines.Git
import           Lines.LinesResult
import           Lines.Options
import           Lines.Prelude
import           Lines.Run
import qualified RIO.Text          as T


linesMain ::
    ( HasLogFunc env
    , HasOptions env
    , HasDefaultOptions env
    , HasProcess env
    , HasExit env
    , HasSystem env
    )
    => RIO env ()
linesMain = do
    tRepo <- oRemoteRepo <$> view optionsL
    tDir <- oTargetDirectory <$> view optionsL
    defaultDir <- defaultCloneDir <$> view defaultOptionsL

    res <- case tRepo of
             Nothing -> do
                case tDir of
                    Nothing  -> exitWithInfo "Invalid usage. You must specify either a directory or a remote repository"
                    Just dir -> do
                         logInfo $ "Running on directory => " <> displayShow dir
                         runLines dir
             Just repo -> do
                 logInfo $ "Cloning and running => " <> displayShow repo
                 _ <- gitClone (T.unpack repo) defaultDir
                 runLines defaultDir



    defaultExists <-  doesDirectoryExist defaultDir
    if defaultExists
        then do
             logInfo $ "done... removing temp dir" <> displayShow defaultDir
             removeDirectory defaultDir
        else do
             pure ()


    logDebug $ displayShow res

    case res of
        NoPaths           -> pure ()
        LineCounts result -> renderResultsAsTable result

    logInfo "done... exiting"
    pure ()
