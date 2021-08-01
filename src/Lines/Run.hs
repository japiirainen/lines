{-# LANGUAGE BlockArguments #-}
module Lines.Run
    ( runLines
    )
    where

import           Lines.Prelude

import           Conduit
import           Lines.App.Class
import           Lines.App.Error
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
                totalFiles = length ps

            pure $ LineCounts $ toLineCountRes (fst result) totalFiles (snd result)


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
                logDebug $ "unsupporter file" <> displayShow filename
                pure Nothing
    where
        isSupportedFile :: Text -> Bool
        isSupportedFile fname = any (True ==) $ map (`T.isSuffixOf` fname) supportedFiles

        go :: Text -> (HasSystem env, HasLogFunc env) => RIO env (Maybe (Text, Int))
        go filepath = do
            content <- readFile filename `catchAny` \e -> do
                logDebug $ "Error reading file" <> displayShow filename <> ": " <> displayShow e
                pure "Invalid file format."
            let nonempty = filter (/=  "") (T.lines content)
                len = Lines.Prelude.length nonempty
            pure $ Just (filepath, len)

        pathSuffix :: Text -> Text
        pathSuffix path = fromMaybe "" (parts path ^? ix (lastItemIndex $ parts path))

        parts :: Text -> [Text]
        parts = T.split (== '/')

        lastItemIndex :: [Text] -> Int
        lastItemIndex p = length p - 1

countLangTotal :: [(Text, [Int])] -> [(Text, Int, Int)]
countLangTotal = map go
    where
        go :: (Text, [Int]) -> (Text, Int, Int)
        go (ext, xs) = (ext, sum xs, length xs)


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

getTotals :: [Maybe (Text, Int)] -> (Int, [(Text, Int, Int)])
getTotals xs = (total, langTotals)
    where
        langTotals :: [(Text, Int, Int)]
        langTotals = countLangTotal grouped
        grouped :: [(Text, [Int])]
        grouped = groupByExtension $ catMaybes xs
        total :: Int
        total = sum $ map snd3 langTotals


snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x
