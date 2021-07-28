module Lines.Table where

import           Lines.Prelude
import           Prelude       (maximum)
import           RIO.List

type ColSize = Int

type Style = String

type Value = String

data Cell = Cell [Style] Value

renderCell :: ColSize -> Cell -> String
renderCell colSize (Cell styles val) = startStyle ++ val ++ padding ++ endStyle
  where
    startStyle = concat styles
    padding = replicate (colSize - length val + 1) ' '
    endStyle = "\x1b[0m"

renderCols :: [[Cell]] -> [[String]]
renderCols = map (intersperse sep) . transpose . map renderCols' . transpose
  where
    getCellVal (Cell _ val) = val
    renderCols' cells = map (renderCell colSize) cells
      where
        colSize = maximum (map (length . getCellVal) cells)

renderRows :: [[String]] -> [String]
renderRows = map ((++ "\n") . concat)

render :: [[Cell]] -> String
render = concat . renderRows . renderCols

-- Utils

sep :: String
sep = renderCell 0 (ext 8 . cell $ "|")

cell :: Value -> Cell
cell = Cell []

defineStyle :: Int -> Int -> Int -> Cell -> Cell
defineStyle color bright shade (Cell styles val) = Cell (styles ++ [style]) val
  where
    bright' = if bright > 0 then ";" ++ show bright else ""
    shade' = if shade > 0 then ";" ++ show shade else ""
    style = "\x1b[" ++ show color ++ bright' ++ shade' ++ "m"

reset :: Cell -> Cell
reset = defineStyle 0 0 0

bold :: Cell -> Cell
bold = defineStyle 1 0 0

underline :: Cell -> Cell
underline = defineStyle 4 0 0

reversed :: Cell -> Cell
reversed = defineStyle 7 0 0

black :: Cell -> Cell
black = defineStyle 30 0 0

red :: Cell -> Cell
red = defineStyle 31 0 0

green :: Cell -> Cell
green = defineStyle 32 0 0

yellow :: Cell -> Cell
yellow = defineStyle 33 0 0

blue :: Cell -> Cell
blue = defineStyle 34 0 0

magenta :: Cell -> Cell
magenta = defineStyle 35 0 0

cyan :: Cell -> Cell
cyan = defineStyle 36 0 0

white :: Cell -> Cell
white = defineStyle 37 0 0

brightBlack :: Cell -> Cell
brightBlack = defineStyle 30 1 0

brightRed :: Cell -> Cell
brightRed = defineStyle 31 1 0

brightGreen :: Cell -> Cell
brightGreen = defineStyle 32 1 0

brightyellow :: Cell -> Cell
brightyellow = defineStyle 33 1 0

brightBlue :: Cell -> Cell
brightBlue = defineStyle 34 1 0

brightMagenta :: Cell -> Cell
brightMagenta = defineStyle 35 1 0

brightCyan :: Cell -> Cell
brightCyan = defineStyle 36 1 0

brightWhite :: Cell -> Cell
brightWhite = defineStyle 37 1 0

bgBlack :: Cell -> Cell
bgBlack = defineStyle 40 0 0

bgRed :: Cell -> Cell
bgRed = defineStyle 41 0 0

bgGreen :: Cell -> Cell
bgGreen = defineStyle 42 0 0

bgYellow :: Cell -> Cell
bgYellow = defineStyle 43 0 0

bgBlue :: Cell -> Cell
bgBlue = defineStyle 44 0 0

bgMagenta :: Cell -> Cell
bgMagenta = defineStyle 45 0 0

bgCyan :: Cell -> Cell
bgCyan = defineStyle 46 0 0

bgWhite :: Cell -> Cell
bgWhite = defineStyle 47 0 0

bgBrightBlack :: Cell -> Cell
bgBrightBlack = defineStyle 40 1 0

bgBrightRed :: Cell -> Cell
bgBrightRed = defineStyle 41 1 0

bgBrightGreen :: Cell -> Cell
bgBrightGreen = defineStyle 42 1 0

bgBrightYellow :: Cell -> Cell
bgBrightYellow = defineStyle 43 1 0

bgBrightBlue :: Cell -> Cell
bgBrightBlue = defineStyle 44 1 0

bgBrightMagenta :: Cell -> Cell
bgBrightMagenta = defineStyle 45 1 0

bgBrightCyan :: Cell -> Cell
bgBrightCyan = defineStyle 46 1 0

bgBrightWhite :: Cell -> Cell
bgBrightWhite = defineStyle 47 1 0

ext :: Int -> Cell -> Cell
ext = defineStyle 38 5
