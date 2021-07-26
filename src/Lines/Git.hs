module Lines.Git
    ( gitClone
    )
    where

import           Lines.App.Class
import           Lines.Prelude

gitClone :: HasProcess env => String -> FilePath -> RIO env ()
gitClone url dir = callProcess "git" ["clone", "--quiet", url, dir]
