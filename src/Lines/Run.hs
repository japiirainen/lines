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
import qualified Prelude
import           RIO.FilePath              (takeExtension, (</>))
import           RIO.List                  (groupBy, sum)
import qualified RIO.Text                  as T

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
    if null [paths]
        then pure NoPaths
        else
         do
            ps <- findFiles [paths]
            let groupedByExtension = groupBy ((==) `on` snd) ps

            lns <- (traverse . traverse) countLinesInFile groupedByExtension

            let totalsByLang = map countLangTotal lns
                totalLines = sum $ map (sum . map snd) lns
                result = toLineCountRes totalLines totalsByLang

            pure $ LineCounts result

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
    => (FilePath, Text) -> RIO env (Text, Int)
countLinesInFile (filename, extension) = do
    content <- readFile filename
    let nonempty = filter (/=  "") (T.lines content)
        len = Lines.Prelude.length nonempty
    pure (extension, len)

countLangTotal :: [(Text, Int)] -> (Text, Int)
countLangTotal xs = (ext, countTotal $ map snd xs)
    where
        ext :: Text
        ext = fst $ Prelude.head xs

        countTotal :: [Int] -> Int
        countTotal = sum


