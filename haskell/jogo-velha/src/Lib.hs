module Lib
    ( someFunc,
      createBoard,
      isNumeric,
      getElem,
      inRange,
      isVictory,
      getAllVictoryPossibilities,
      isBoardFull,
      Cell,
      Board
    ) where

import Data.Char (isDigit)

type Cell = String
type Board = [[Cell]]

createBoard :: Board
createBoard = replicate 3 (replicate 3 ".")

isNumeric :: String -> Bool
isNumeric = all isDigit

getElem :: Board -> [Int] -> Cell
getElem board indexes = do
    let i = (indexes !! 0)
    let j = (indexes !! 1)
    ((board !! i) !! j)

inRange :: Int -> Int -> Int -> Bool
inRange num start end = (num >= start) && (num <= end)

getColumns :: Board -> Board
getColumns ([]:_) = []
getColumns matrix = (map head matrix) : getColumns (map tail matrix)

getPrimaryDiagonal :: Board -> [Cell]
getPrimaryDiagonal board = [(getElem board [index, index]) | index <- [0..2]]

getSecondaryDiagonal :: Board -> [Cell]
getSecondaryDiagonal board = [(getElem board [index, (3 - (index + 1))]) | index <- [0..2]]

getAllVictoryPossibilities :: Board -> Board
getAllVictoryPossibilities board = board ++ (getColumns board) ++ [(getPrimaryDiagonal board), (getSecondaryDiagonal board)]

isVictory :: [Cell] -> Cell -> Bool
isVictory possibilitie lastPlayer = all (== lastPlayer) possibilitie

isBoardFull :: Board -> Bool
isBoardFull board = all (/= ".") (concat board)

someFunc :: IO()
someFunc = putStrLn "someFunc"
