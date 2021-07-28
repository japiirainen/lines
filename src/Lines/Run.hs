{-# LANGUAGE BlockArguments #-}
module Lines.Run
    ( runLines
    )
    where

import           Lines.Prelude

import           Conduit
import           Lines.App.Class
import           Lines.LinesResult
import qualified Prelude
import           RIO.FilePath      (takeExtension)
import           RIO.List          (groupBy, sortBy)
import qualified RIO.Text          as T

runLines :: HasSystem env => FilePath -> RIO env LinesResult
runLines paths = do
    if null [paths]
        then pure NoPaths
        else
         do
            ps <- allFiles paths
            lns <- traverse countLinesInFile ps

            let groupedByExtension = groupByExtension $ catMaybes lns
                totalsByLang = countLangTotal groupedByExtension
                totalLines = sum $ map snd totalsByLang
                result = toLineCountRes totalLines totalsByLang

            pure $ LineCounts result


isPathHidden :: FilePath -> Bool
isPathHidden = T.isInfixOf "/." . T.pack

isLockFile :: FilePath -> Bool
isLockFile = T.isInfixOf ".lock" . T.pack

countLinesInFile
    :: ( HasSystem env
       )
    => (FilePath, Text) -> RIO env (Maybe (Text, Int))
countLinesInFile (filename, extension) = do
    if any (extension ==) supportedLanguageExtensions
        then do
            content <- readFile filename
            let nonempty = filter (/=  "") (T.lines content)
                len = Lines.Prelude.length nonempty
            pure $ Just (extension, len)
        else pure Nothing

countLangTotal :: [(Text, [Int])] -> [(Text, Int)]
countLangTotal = map go
    where
        go :: (Text, [Int]) -> (Text, Int)
        go (ext, xs) = (ext, sum xs)



allFiles :: FilePath -> RIO env [(FilePath, Text)]
allFiles path =
    runConduitRes $
            sourceDirectoryDeep True path
            .| filterC (not . isPathHidden)
            .| filterC (not . isLockFile)
            .| mapC (\p -> (p, T.pack $ takeExtension p))
            .| sinkList


groupByExtension :: [(Text, Int)] -> [(Text, [Int])]
groupByExtension = map (\l -> (fst . Prelude.head $ l, map snd l)) . groupBy ((==) `on` fst)
          . sortBy (comparing fst)
