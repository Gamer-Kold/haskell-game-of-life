module Main where

import Raylib.Core
  ( beginDrawing,
    beginMode2D,
    clearBackground,
    closeWindow,
    endDrawing,
    endMode2D,
    initWindow,
    getMouseDelta,
    isMouseButtonDown,
    getMousePosition,
    getMouseWheelMove,
    getScreenToWorld2D,
    setTargetFPS,
  )
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Util (whileWindowOpen)
import Raylib.Util.Colors (lightGray, rayWhite)
import Raylib.Types (Vector2(Vector2), Camera2D(Camera2D, camera2D'offset, camera2D'zoom, camera2D'target, camera2D'rotation), MouseButton(MouseButtonRight))
import Raylib.Util.Math((|+|), (|*))

data CellState = Alive | Dead deriving (Eq, Show)

type Point = (Int, Int)

type Cell = (Point, CellState) -- I hate this.

type Grid = [Cell]

type FrameNo = Int

type State = (FrameNo, Grid, Camera2D)

cam = Camera2D (Vector2 0 0) (Vector2 0 0) 0 1

main :: IO ()
main =
  let initialState = (0, stringToGrid "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDAAADDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD", cam)
   in do
        window <- initWindow 500 500 "Game Of Life"
        setTargetFPS 60
        whileWindowOpen
          ( \a -> do
              scroll <- getMouseWheelMove
              mousePos <- getMousePosition
              mouseDelta <- getMouseDelta
              mouseWorldPos <- getScreenToWorld2D mousePos $ third a
              rightMouseButtonDown <- isMouseButtonDown MouseButtonRight
              let newZoom = max (if scroll /= 0 then camera2D'zoom (third a) + (scroll * 0.125) else camera2D'zoom (third a)) 0.125
              let newOffset = if scroll /= 0 then mousePos else camera2D'offset (third a)
              let newTarget = if scroll /= 0 then mouseWorldPos else if rightMouseButtonDown then (camera2D'target $ third a) |+| (mouseDelta |* (-1 / (camera2D'zoom $ third a))) else camera2D'target $ third a
              beginDrawing
              beginMode2D $ third a
              clearBackground rayWhite
              -- drawText "Basic raylib window" 30 40 18 lightGray
              let newGrid = if (first a `mod` 60) == 0 then filter (\x -> snd x == Alive) (mutateGrid $ second a) else second a
              let newFrameNo = if first a > 60 then 1 else first a + 1
              drawGrid newGrid
              putStrLn $ show $ second a
              endMode2D
              endDrawing
              return (newFrameNo , newGrid, Camera2D newOffset newTarget (camera2D'rotation $ third a) newZoom)
          )
          initialState

        closeWindow window


first (x, _, _) = x
second (_, x, _) = x
third (_, _, x) = x

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
