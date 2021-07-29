module Lines.LinesResult
    ( LinesResult(..)
    , toLineCountRes
    , langCountToString
    , totalCountToString
    , renderResultsAsTable
    , supportedLanguageExtensions
    , supportedLanguages
    , supportedFiles
    )
    where

import           Data.Bifunctor
import           Lines.Prelude
import           Lines.Table
import qualified Prelude

newtype TotalCount = TotalCount Int
    deriving stock Show
newtype LangCount = LangCount Int
    deriving stock Show


data LinesResult
    = NoPaths
    | LineCounts LineCountRes
    deriving stock Show

data LineCountRes
    = LineCountRes
    { total             :: TotalCount
    , resultsByLanguage :: [(Language, LangCount)]
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
    deriving stock (Show, Eq)

supportedLanguages :: [Language]
supportedLanguages = [Haskell, Purescript, FSharp, Typescript, Javascript, Java, Scala, Ruby, Python, C, CPP, GraphQL, Unknown, TypescriptReact, JavascriptReact, Shell, JSON, YAML, YML, Markdown, Cabal]

supportedLanguageExtensions :: [Text]
supportedLanguageExtensions = [".hs", ".purs", ".fs", ".ts", ".js", ".java", ".scala", ".rb", ".py", ".c", ".cpp", ".graphql", ".unknown", ".tsx", ".jsx", ".sh", ".json", ".yaml", ".yml", ".md", ".cabal"]

supportedFiles :: [Text]
supportedFiles = ["dockerfile", "Dockerfile"]

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
    "dockerfile" -> Dockerfile
    "Dockerfile" -> Dockerfile
    _            -> Unknown


totalCountToString :: TotalCount -> String
totalCountToString (TotalCount n) = show n

langCountToString :: LangCount -> String
langCountToString (LangCount n) = show n

toLineCountRes :: Int -> [(Text, Int)] -> LineCountRes
toLineCountRes total rs =
    LineCountRes
        { total = TotalCount total
        , resultsByLanguage = toRes rs
        }
    where
        toRes :: [(Text, Int)] -> [(Language, LangCount)]
        toRes = map (bimap extToLanguage LangCount)



resultToTable :: LineCountRes -> String
resultToTable res = render $ head : body <> foot
    where
        head = map (underline . bold . cell) ["LANGUAGE", "LINES COUNT"]
        body = map rows $ resultsByLanguage res
        rows :: (Language, LangCount) -> [Cell]
        rows (lang, count) =
            [ blue $ cell $ show lang
            , green $ cell $ langCountToString count
            ]
        foot =
            [ replicate 2 . cell $ "--------",
                [ red $ bold . cell $ "TOTAL LINES"
                , red $ bold . cell $ totalCountToString $ total res
                ]
            ]


renderResultsAsTable :: LineCountRes -> RIO env ()
renderResultsAsTable res = liftIO $ Prelude.putStrLn $ "\n" <> resultToTable res
