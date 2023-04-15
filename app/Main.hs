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
import Raylib.Util (whileWindowOpen)
import Raylib.Util.Colors (lightGray, rayWhite)

data CellState = Alive | Dead deriving (Eq, Show)

type Point = (Int, Int)

type Cell = (Point, CellState) -- I hate this.

type Grid = [Cell]

type FrameNo = Int

type State = (FrameNo, Grid)

main :: IO ()
main =
  let initialState = (0, stringToGrid "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDAAADDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
   in do
        window <- initWindow 500 500 "Game Of Life"
        setTargetFPS 60
        whileWindowOpen
          ( \a -> do
              beginDrawing

              clearBackground rayWhite
              -- drawText "Basic raylib window" 30 40 18 lightGray

              let newGrid = if (fst a `mod` 60) == 0 then filter (\x -> snd x == Alive) (mutateGrid $ snd a) else snd a
              drawGrid newGrid
              endDrawing
              return (fst a + 1, newGrid)
          )
          initialState

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
stringToGrid str = reverse $ foldl (\acc c -> cell (((length acc)) `mod` 10) (((length acc)) `div` 10) (charToCellState c) : acc) [(cell 0 0 (charToCellState (head str)))] (tail str)

getCellFromGrid :: Point -> Grid -> Cell
getCellFromGrid point grid = case filter (\x -> fst x == point) grid of
  x : xs -> x
  [] -> (point, Dead)

adjacentCells :: Point -> Grid -> [Cell]
adjacentCells point grid = [getCellFromGrid (fst point + x, snd point + y) grid | x <- [-1, 0, 1], y <- [-1, 0, 1]] 

newStateFromNoAliveNeighbours :: Int -> CellState
newStateFromNoAliveNeighbours 3 = Alive
newStateFromNoAliveNeighbours x = Dead

mutateGrid :: Grid -> Grid
mutateGrid [] = []
mutateGrid (x:xs) = (map (\a -> mutateCell (x:xs) (fst a)) (adjacentCells (fst x) (x:xs))) ++ mutateGrid xs

mutateCell grid point = (point, (newStateFromNoAliveNeighbours (length $ filter (\y -> snd y == Alive) (adjacentCells (point) grid))))
