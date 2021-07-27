{-# LANGUAGE BlockArguments #-}
module Lines.Run
    ( findFiles
    , runLines
    )
    where

import           Lines.Prelude

import           Conduit
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Lines.App.Class
import           Lines.LinesResult
import           Lines.Options
import           RIO.FilePath              (takeExtension, (</>))
import           RIO.Text                  as T hiding (concat, filter, map)

runLines
    :: ( HasLogFunc env
       , HasOptions env
       , HasSystem env
       , HasSystem env
       , HasProcess env
       )
    => FilePath
    -> RIO env LinesResult
runLines paths = do
    ps <- findFiles [paths]

    ls <- traverse countLinesInFile $ fst <$> ps

    logDebug $ "lengths" <> displayShow ls

    pure NoPaths

findFiles :: HasSystem env => [FilePath] -> RIO env [(FilePath, Text)]
findFiles = fmap concat . traverse go
    where
        go :: HasSystem env => FilePath -> RIO env [(FilePath, Text)]
        go parent = do
            isDirectory <- doesDirectoryExist parent

            if isDirectory
                then do
                    files <- listDirectory parent
                    findFiles $ map (parent </>) files
                else fmap maybeToList $ runMaybeT $ do
                    guardM $ lift $ doesFileExist parent
                    guardM $ lift $ not <$> isFileSymbolicLink parent
                    pure (parent, T.pack $ takeExtension parent)

countLinesInFile
    :: ( HasSystem env
       )
    => FilePath -> RIO env Int
countLinesInFile filename = do
    content <- readFile filename
    let nonempty = filter (/=  "") (T.lines content)
        len = Lines.Prelude.length nonempty
    pure len
