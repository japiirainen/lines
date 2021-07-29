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
import           RIO.Lens
import           RIO.List          (groupBy, sortBy)
import qualified RIO.Text          as T

runLines :: (HasSystem env, HasLogFunc env) => FilePath -> RIO env LinesResult
runLines paths = do
    if null [paths]
        then pure NoPaths
        else
         do
            ps <- allFiles paths
            lns <- traverse countLinesInFile ps
            let result = getTotals lns

            pure $ LineCounts $ toLineCountRes (fst result) (snd result)


isPathHidden :: FilePath -> Bool
isPathHidden = T.isInfixOf "/." . T.pack

isLockFile :: FilePath -> Bool
isLockFile = T.isInfixOf ".lock" . T.pack

countLinesInFile
    :: ( HasSystem env
       , HasLogFunc env
       )
    => (FilePath, Text) -> RIO env (Maybe (Text, Int))
countLinesInFile (filename, extension) = do
    if any (extension ==) supportedLanguageExtensions
        then go extension
        else do
            if isSupportedFile $ T.pack filename
                then go $ pathSuffix $ T.pack filename
            else do
                logError $ "unsupporter file" <> displayShow filename
                pure Nothing
    where
        isSupportedFile :: Text -> Bool
        isSupportedFile fname = any (True ==) $ map (`T.isSuffixOf` fname) supportedFiles
        go :: Text -> (HasSystem env) => RIO env (Maybe (Text, Int))
        go filepath = do
            content <- readFile filename
            let nonempty = filter (/=  "") (T.lines content)
                len = Lines.Prelude.length nonempty
            pure $ Just (filepath, len)
        pathSuffix :: Text -> Text
        pathSuffix path = fromMaybe "" (parts path ^? ix (lastItemIndex $ parts path))
        parts :: Text -> [Text]
        parts = T.split (== '/')
        lastItemIndex :: [Text] -> Int
        lastItemIndex p = length p - 1

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

getTotals :: [Maybe (Text, Int)] -> (Int, [(Text, Int)])
getTotals xs = (total, langTotals)
    where
        langTotals :: [(Text, Int)]
        langTotals = countLangTotal grouped
        grouped :: [(Text, [Int])]
        grouped = groupByExtension $ catMaybes xs
        total :: Int
        total = sum $ map snd langTotals
