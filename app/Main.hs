module Main where

import Raylib.Core
  ( beginDrawing,
    clearBackground,
    closeWindow,
    endDrawing,
    initWindow,
    setTargetFPS,
  )
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Util (whileWindowOpen0)
import Raylib.Util.Colors (lightGray, rayWhite)

data CellState = Alive | Dead deriving (Eq, Show)

type Point = (Int, Int)

type Cell = (Point, CellState) -- I hate this.

type Grid = [Cell]

main :: IO ()
main = do
  window <- initWindow 500 500 "Game Of Life"
  setTargetFPS 60

  whileWindowOpen0
    ( do
        beginDrawing

        clearBackground rayWhite
        -- drawText "Basic raylib window" 30 40 18 lightGray

        drawGrid (stringToGrid "DDDADDDAAADDDADDDAAADDDADDDAAADDDADDDAAADDDADDDAAADDDADDDAAADDDADDDAAADDDADDDAAADDDADDDAAADDDADDDAAADDDADDDAAA")

        endDrawing
    )

  closeWindow window

drawGrid :: Grid -> IO [()]
drawGrid grid = sequence (map (\x -> drawRectangle ((fst (fst x)) * 50) ((snd (fst x)) * 50) 50 50 lightGray) (filter (\x -> (snd x) == Alive) grid))

cell :: Int -> Int -> CellState -> Cell
cell x y state = ((x, y), state)

deadGrid :: Int -> Int -> Grid
deadGrid width height = [cell x y Dead | x <- [0 .. width], y <- [0 .. height]]

charToCellState :: Char -> CellState
charToCellState 'A' = Alive
charToCellState 'D' = Dead

stringToGrid :: [Char] -> Grid
stringToGrid str = foldl (\acc c -> cell (((length acc)) `mod` 10) (((length acc)) `div` 10) (charToCellState c) : acc) [(cell 0 0 (charToCellState (head str)))] (tail str)
