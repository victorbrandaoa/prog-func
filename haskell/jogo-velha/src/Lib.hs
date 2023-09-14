module Lib
    ( someFunc,
      createBoard,
      isNumeric,
      getElem,
      inRange,
      isVictory,
      getAllVictoryPossibilities,
      isBoardFull
    ) where

import Data.Char (isDigit)

createBoard :: [[String]]
createBoard = [[".", ".", "."], [".", ".", "."],[".", ".", "."]]

isNumeric :: String -> Bool
isNumeric [] = False
isNumeric move = all isDigit move

getElem :: [[String]] -> [Int] -> String
getElem board indexes = do
    let i = (indexes !! 0)
    let j = (indexes !! 1)
    ((board !! i) !! j)

inRange :: Int -> Int -> Int -> Bool
inRange num start end = (num >= start) && (num <= end)

getColumns :: [[String]] -> [[String]]
getColumns ([]:_) = []
getColumns matrix = (map head matrix) : getColumns (map tail matrix)

getPrimaryDiagonal :: [[String]] -> [String]
getPrimaryDiagonal board = [(getElem board [index, index]) | index <- [0..2]]

getSecondaryDiagonal :: [[String]] -> [String]
getSecondaryDiagonal board = [(getElem board [index, (3 - (index + 1))]) | index <- [0..2]]

getAllVictoryPossibilities :: [[String]] -> [[String]]
getAllVictoryPossibilities board = board ++ (getColumns board) ++ [(getPrimaryDiagonal board), (getSecondaryDiagonal board)]

isVictory :: [String] -> String -> Bool
isVictory possibilitie lastPlayer = all (\e -> e == lastPlayer) possibilitie

isBoardFull :: [[String]] -> Bool
isBoardFull board = all (\e -> e /= ".") (concat board)

someFunc :: IO()
someFunc = putStrLn "someFunc"
