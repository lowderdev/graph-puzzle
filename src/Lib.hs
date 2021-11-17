module Lib
  ( run,
  )
where

import           Cell
import           Grid (Grid, blankGrid, printGrid, randomGrid, rotateCell)


run :: IO ()
run = runGame randomGrid

runGame :: Grid Cell -> IO ()
runGame grid = do
  printGrid grid
  printMovePrompt
  cellNum <- getLine
  updateGrid grid $ validateMove grid cellNum

printTurn :: Cell -> IO ()
printTurn cell = putStrLn $ show cell ++ "'s turn"

printMovePrompt :: IO ()
printMovePrompt = putStr "Enter number 1-4: "

validateMove :: Grid Cell -> String -> Either String String
validateMove grid cellNum
  | invalidCellNum = Left "Move is not a num 1-4"
  | otherwise = Right cellNum
  where
    invalidCellNum = cellNum `notElem` map show [1 .. 4]

updateGrid :: Grid Cell -> Either String String -> IO ()
updateGrid grid (Left error) = putStrLn error >> runGame grid
updateGrid grid (Right cellNum) = runGame nextGrid
  -- | detectWin nextGrid = printGrid nextGrid >> printWinMessage
  -- | detectTie nextGrid = printGrid nextGrid >> printTieMessage
  -- | otherwise = runGame nextGrid (nextCell)
  where
    nextGrid = rotateCell cellNum grid

-- printWinMessage :: Cell -> IO ()
-- printWinMessage cell = putStrLn $ concat ["===== ", show cell, " Wins!", " ====="]
