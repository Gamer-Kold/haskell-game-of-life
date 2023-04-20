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
    isMouseButtonPressed,
    isKeyPressed,
    getMousePosition,
    getMouseWheelMove,
    getScreenToWorld2D,
    setTargetFPS,
  )
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Util (whileWindowOpen)
import Raylib.Util.Colors (lightGray, rayWhite)
import Raylib.Types (Vector2(Vector2, vector2'x, vector2'y), Camera2D(Camera2D, camera2D'offset, camera2D'zoom, camera2D'target, camera2D'rotation), MouseButton(MouseButtonLeft, MouseButtonRight), KeyboardKey(KeyP))
import Raylib.Util.Math((|+|), (|*))

data CellState = Alive | Dead deriving (Eq, Show)
data Mode = Build | Play deriving(Eq, Show)

type Point = (Int, Int)

type Cell = (Point, CellState) -- I hate this.

type Grid = [Cell]

type FrameNo = Int

type State = (FrameNo, Grid, Camera2D, Mode)

cam = Camera2D (Vector2 0 0) (Vector2 0 0) 0 1

main :: IO ()
main =
  let initialState = (0, stringToGrid "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDAAADDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD", cam, Build)
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
              leftMouseButtonDown <- isMouseButtonPressed MouseButtonLeft
              pKeyDown <- isKeyPressed KeyP
              let mouseGridPos = (floor (vector2'x mouseWorldPos / 50), floor (vector2'y mouseWorldPos / 50)) -- I have no fucking clue how this line of code works, I saw it in a dream -- I have no fucking clue how this line of code works, I saw it in a dream.
              let newGrid = if fourth a == Play then if (first a `mod` 60) == 0 then filter (\x -> snd x == Alive) (mutateGrid $ second a) else second a else if leftMouseButtonDown then setCellInGrid mouseGridPos (second a) (if snd (getCellFromGrid mouseGridPos (second a)) == Alive then Dead else Alive) else second a
              let newFrameNo = if first a > 60 then 1 else first a + 1
              let newMode = if pKeyDown then if fourth a == Build then Play else Build else fourth a
              drawGrid newGrid
              endMode2D
              endDrawing
              return (newFrameNo , newGrid, Camera2D newOffset newTarget (camera2D'rotation $ third a) newZoom, newMode)
          )
          initialState

        closeWindow window


first (x, _, _, _) = x
second (_, x, _, _) = x
third (_, _, x, _) = x
fourth (_, _, _, x) = x

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

setCellInGrid :: Point -> Grid -> CellState -> Grid
setCellInGrid point grid state = (filter (\x -> fst x /= point) grid) ++ [(point, state)]

adjacentCells :: Point -> Grid -> [Cell]
adjacentCells point grid = [getCellFromGrid (fst point + x, snd point + y) grid | x <- [-1, 0, 1], y <- [-1, 0, 1]] 

newStateFromNoAliveNeighbours :: Int -> CellState
newStateFromNoAliveNeighbours 3 = Alive
newStateFromNoAliveNeighbours x = Dead

mutateGrid :: Grid -> Grid
mutateGrid [] = []
mutateGrid (x:xs) = removeDuplicatesFromGrid $ (map (\a -> mutateCell (x:xs) (fst a)) (adjacentCells (fst x) (x:xs))) ++ mutateGrid xs

removeDuplicatesFromGrid :: Grid -> Grid 
removeDuplicatesFromGrid [] = []
removeDuplicatesFromGrid (x:xs) = x : removeDuplicatesFromGrid (filter (\a -> fst a /= fst x) xs)

mutateCell grid point = (point, (newStateFromNoAliveNeighbours (length $ filter (\y -> snd y == Alive) (adjacentCells (point) grid))))
