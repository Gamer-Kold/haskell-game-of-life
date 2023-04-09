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

main :: IO ()
main = do
  window <- initWindow 500 500 "raylib [core] example - basic window"
  setTargetFPS 60

  whileWindowOpen0
    ( do
        beginDrawing

        clearBackground rayWhite
        -- drawText "Basic raylib window" 30 40 18 lightGray
        
        drawGridRecursively 0 0 50

        endDrawing
    )

  closeWindow window

drawGridRecursively :: Int -> Int -> Int -> IO ()
drawGridRecursively cellX cellY cellSize 
  | cellX > 500 = drawGridRecursively 0 (cellY + cellSize) cellSize -- not general :vomit:
  | cellY > 500 = return ()
  | otherwise = do
    drawRectangle cellX cellY cellSize cellSize lightGray
    drawGridRecursively (cellX + cellSize) cellY cellSize
