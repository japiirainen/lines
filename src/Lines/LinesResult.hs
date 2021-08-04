module Lines.LinesResult
    ( LinesResult(..)
    , toLineCountRes
    , langCountToString
    , totalCountToString
    , renderResultsAsTable
    , supportedLanguageExtensions
    , supportedLanguages
    , supportedFiles
    , isFilePathBlackListed
    )
    where

import           Lines.Prelude
import           Lines.Table
import qualified Prelude
import qualified RIO.Text      as T

newtype TotalCount = TotalCount Int
    deriving stock Show

newtype LangLinesCount = LangLinesCount Int
    deriving stock Show

newtype FilesCount = FilesCount Int
    deriving stock Show

newtype LangFilesCount = LangFilesCount Int
    deriving stock Show

newtype LanguagesCount = LanguagesCount Int
    deriving stock Show

data LinesResult
    = NoPaths
    | LineCounts LineCountRes
    deriving stock Show

data LineCountRes
    = LineCountRes
    { totalLines        :: TotalCount
    , totalFiles        :: FilesCount
    , totalLanguages    :: LanguagesCount
    , resultsByLanguage :: [(Language, LangLinesCount, LangFilesCount)]
    }
    deriving stock Show

data Language
    = Haskell
    | Purescript
    | FSharp
    | Typescript
    | Javascript
    | Java
    | Scala
    | Ruby
    | Python
    | C
    | CPP
    | GraphQL
    | Unknown
    | TypescriptReact
    | JavascriptReact
    | Shell
    | JSON
    | YAML
    | YML
    | Markdown
    | Cabal
    | SQL
    | Dockerfile
    | Txt
    | LICENSE
    | Css
    | HTML
    | HeaderFile
    | CSharp
    | Elm
    | Clojure
    | ClojureScript
    deriving stock (Show, Eq)

supportedLanguages :: [Language]
supportedLanguages =
                    [ Haskell
                    , Purescript
                    , FSharp
                    , Typescript
                    , Javascript
                    , Java
                    , Scala
                    , Ruby
                    , Python
                    , C
                    , CPP
                    , GraphQL
                    , Unknown
                    , TypescriptReact
                    , JavascriptReact
                    , Shell
                    , JSON
                    , YAML
                    , YML
                    , Markdown
                    , Cabal
                    , Txt
                    , Css
                    , HTML
                    , HeaderFile
                    , CSharp
                    , Elm
                    , Clojure
                    , ClojureScript
                    ]

supportedLanguageExtensions :: [Text]
supportedLanguageExtensions =
                             [ ".hs"
                             , ".purs"
                             , ".fs"
                             , ".ts"
                             , ".js"
                             , ".java"
                             , ".scala"
                             , ".rb"
                             , ".py"
                             , ".c"
                             , ".cpp"
                             , ".graphql"
                             , ".unknown"
                             , ".tsx"
                             , ".jsx"
                             , ".sh"
                             , ".json"
                             , ".yaml"
                             , ".yml"
                             , ".md"
                             , ".cabal"
                             , ".txt"
                             , ".css"
                             , ".html"
                             , ".h"
                             , ".cs"
                             , ".elm"
                             , ".clj"
                             , ".cljs"
                             ]

supportedFiles :: [Text]
supportedFiles =
                [ "dockerfile"
                , "Dockerfile"
                , "LICENSE"
                ]

extToLanguage :: Text -> Language
extToLanguage = \case
    ".hs"        -> Haskell
    ".purs"      -> Purescript
    ".fs"        -> FSharp
    ".ts"        -> Typescript
    ".js"        -> Javascript
    ".java"      -> Java
    ".scala"     -> Scala
    ".rb"        -> Ruby
    ".py"        -> Python
    ".c"         -> C
    ".cpp"       -> CPP
    ".graphql"   -> GraphQL
    ".tsx"       -> TypescriptReact
    ".jsx"       -> JavascriptReact
    ".sh"        -> Shell
    ".json"      -> JSON
    ".yaml"      -> YAML
    ".yml"       -> YML
    ".md"        -> Markdown
    ".cabal"     -> Cabal
    ".sql"       -> SQL
    ".txt"       -> Txt
    ".css"       -> Css
    ".html"      -> HTML
    ".h"         -> HeaderFile
    ".cs"        -> CSharp
    ".elm"       -> Elm
    ".clj"       -> Clojure
    ".cljs"      -> ClojureScript
    "dockerfile" -> Dockerfile
    "Dockerfile" -> Dockerfile
    "LICENSE"    -> LICENSE
    _            -> Unknown


isFilePathBlackListed :: [Text] -> FilePath -> Bool
isFilePathBlackListed toIgnore p = any (True ==) $ map (`T.isInfixOf` T.pack p) toIgnore

totalCountToString :: TotalCount -> String
totalCountToString (TotalCount n) = show n

langCountToString :: LangLinesCount -> String
langCountToString (LangLinesCount n) = show n

langFilesCountToString :: LangFilesCount -> String
langFilesCountToString (LangFilesCount n) = show n

filesCountToString :: FilesCount -> String
filesCountToString (FilesCount n) = show n

languagesCountToString :: LanguagesCount -> String
languagesCountToString (LanguagesCount n) = show n

toLineCountRes :: Int -> Int -> [(Text, Int, Int)] -> LineCountRes
toLineCountRes totalLines totalFiles rs =
    LineCountRes
        { totalLines = TotalCount totalLines
        , totalFiles = FilesCount totalFiles
        , resultsByLanguage = toRes rs
        , totalLanguages = LanguagesCount languagesCount
        }
    where
        toRes :: [(Text, Int, Int)] -> [(Language, LangLinesCount, LangFilesCount)]
        toRes = map (\(lang, lns, files) -> (extToLanguage lang, LangLinesCount lns, LangFilesCount files))
        languagesCount :: Int
        languagesCount = length rs



resultToTable :: LineCountRes -> String
resultToTable res = render $ head : body <> foot
    where
        head = map (underline . bold . cell) ["LANGUAGE", "LINES COUNT", "FILES COUNT"]
        body = map rows $ resultsByLanguage res
        rows :: (Language, LangLinesCount, LangFilesCount) -> [Cell]
        rows (lang, linesCount, filesCount) =
            [ blue $ cell $ show lang
            , green $ cell $ langCountToString linesCount
            , magenta $ cell $ langFilesCountToString filesCount
            ]
        foot =
            [ replicate 3 . cell $ "------------",
                [ brightMagenta $ bold . cell $ languagesCount
                , brightGreen $ bold . cell $ linesTotal
                , brightMagenta $ bold . cell $ filesTotal
                ]
            ]
        languagesCount = languagesCountToString $ totalLanguages res
        linesTotal = totalCountToString $ totalLines res
        filesTotal = filesCountToString $ totalFiles res


renderResultsAsTable :: LineCountRes -> RIO env ()
renderResultsAsTable res = liftIO $ Prelude.putStrLn $ "\n" <> resultToTable res
