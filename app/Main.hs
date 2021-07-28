module Main where

import           Lines.App
import           Lines.Main
import           Lines.Options
import           Lines.Prelude

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    options <- parseOptions

    let defaultOptions = DefaultOptions { defaultCloneDir = "lines-temp" }
    app <- bootstrapApp options defaultOptions

    runRIO app linesMain

