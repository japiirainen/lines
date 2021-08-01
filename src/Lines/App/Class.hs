module Lines.App.Class
    ( HasProcess(..)
    , HasExit(..)
    , HasSystem(..)
    , exitWithInfo
    )
    where

import           Lines.Prelude


class HasSystem env where
    getCurrentDirectory :: RIO env FilePath
    setCurrentDirectory :: FilePath -> RIO env ()
    doesFileExist :: FilePath -> RIO env Bool
    doesDirectoryExist :: FilePath -> RIO env Bool
    isFileSymbolicLink :: FilePath -> RIO env Bool
    listDirectory :: FilePath -> RIO env [FilePath]
    readFile :: FilePath -> RIO env Text
    readFileBS :: FilePath -> RIO env ByteString
    writeFile :: FilePath -> Text -> RIO env ()
    removeDirectory :: FilePath -> RIO env ()

class HasProcess env where
    callProcess :: String -> [String] -> RIO env ()
    callProcessExitCode :: String -> [String] -> RIO env ExitCode
    readProcess :: String -> [String] -> String -> RIO env String


class HasExit env where
    exitSuccess :: RIO env a


exitWithInfo :: (HasLogFunc env, HasExit env) => Utf8Builder -> RIO env a
exitWithInfo msg = do
    logInfo msg
    exitSuccess
