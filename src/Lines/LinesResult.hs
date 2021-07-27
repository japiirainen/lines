module Lines.LinesResult
    ( LinesResult(..)
    , toLineCountRes
    , langCountToString
    , totalCountToString
    )
    where

import           Data.Bifunctor
import           Lines.Prelude

newtype TotalCount = TotalCount Int
newtype LangCount = LangCount Int


data LinesResult
    = NoPaths
    | LineCounts LineCountRes

data LineCountRes
    = LineCountRes
    { total             :: TotalCount
    , resultsByLanguage :: [(Language, LangCount)]
    }

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
    deriving stock Show

extToLanguage :: Text -> Language
extToLanguage = \case
    ".hs"    -> Haskell
    ".purs"  -> Purescript
    ".fs"    -> FSharp
    ".ts"    -> Typescript
    ".js"    -> Javascript
    ".java"  -> Java
    ".scala" -> Scala
    ".rb"    -> Ruby
    ".py"    -> Python
    ".c"     -> C
    ".cpp"   -> CPP
    -- TODO handle this
    _        -> error "unknown language"


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

