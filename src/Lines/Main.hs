module Lines.Main
    (linesMain
    )
    where

import           Lines.App.Class
import           Lines.Logger
import           Lines.Options
import           Lines.Prelude
import           Lines.Run


linesMain ::
    ( HasLogFunc env
    , HasOptions env
    , HasProcess env
    , HasExit env
    , HasSystem env
    )
    => RIO env a
linesMain = do
    tDir <- oTargetDirectory <$> view optionsL

    res <- case tDir of
            Nothing  -> exitWithInfo "no path"
            Just dir -> runLines dir

    logDebug $ displayShow res

    exitWithInfo "Run succesfull"
