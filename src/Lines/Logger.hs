module Lines.Logger
  ( linesLogFunc,
  )
where

import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import Lines.Options
import Lines.Prelude
import System.Console.ANSI

linesLogFunc :: Options -> LogFunc
linesLogFunc Options {..} = mkLogFunc $ \_cs _source level msg ->
  when (level >= oLogLevel) $ do
    BS8.putStr "["
    when oLogColor $ setSGR [levelStyle level]
    BS8.putStr $ levelStr level
    when oLogColor $ setSGR [Reset]
    BS8.putStr "]"
    BS8.putStrLn $ toStrictBytes $ toLazyByteString $ getUtf8Builder msg

levelStr :: LogLevel -> ByteString
levelStr = \case
  LevelDebug -> "Debug"
  LevelInfo -> "Info"
  LevelWarn -> "Warn"
  LevelError -> "Error"
  LevelOther x -> encodeUtf8 x

levelStyle :: LogLevel -> SGR
levelStyle = \case
  LevelDebug -> SetColor Foreground Dull Magenta
  LevelInfo -> SetColor Foreground Dull Blue
  LevelWarn -> SetColor Foreground Dull Yellow
  LevelError -> SetColor Foreground Dull Red
  LevelOther _ -> Reset
