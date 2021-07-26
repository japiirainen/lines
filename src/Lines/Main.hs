module Lines.Main
    (linesMain
    )
    where

import           Lines.App.Class
import           Lines.Logger
import           Lines.Options
import           Lines.Prelude


linesMain ::
    ( HasLogFunc env
    , HasOptions env
    , HasProcess env
    , HasExit env
    , HasSystem env
    )
    => RIO env a
linesMain = do
    opts <- view optionsL

    logDebug $ "opts" <> displayShow opts

    exitWithInfo "Run succesfull"
