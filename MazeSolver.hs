module MazeSolver where

import           Data.Map (Map, (!), lookup, insert, fromList, adjust)
import qualified Data.Map as Map
import System.Environment

type Path = [Pos]
type Pos  = (Row, Col)
type Row  = Int
type Col  = Int
type Maze = Map Pos Tile
data Tile = Empty | Wall | Start | Finish | Visited
  deriving (Show, Eq)

loadMaze :: String -> IO (Maze, Int, Int, Pos)
loadMaze mazeFile = do
  mazeRows <- lines <$> readFile mazeFile
  let numRows = length mazeRows
  let numCols = length (head mazeRows)
  return $ foldr processRow 
                  (Map.empty, numRows, numCols, (-1, -1)) 
                  (zip [0..(numRows - 1)] mazeRows)

processRow :: (Int, String) -> (Maze, Int, Int, Pos) -> (Maze, Int, Int, Pos)
processRow (i, mazeRow) (currMaze, numRows, numCols, startPos) =
  foldr (processChar i) 
          (currMaze, numRows, numCols, startPos)
          (zip [0..(numCols - 1)] mazeRow)

processChar :: Int -> (Int, Char) -> (Maze, Int, Int, Pos) -> (Maze, Int, Int, Pos)
processChar i (j, mazeChar) (currMaze, numRows, numCols, startPos) = 
  let
    tile = readTile mazeChar
    newMaze = Map.insert (i, j) tile currMaze
  in
    if tile == Start then 
      (newMaze, numRows, numCols, (i, j))
    else
      (newMaze, numRows, numCols, startPos)

readTile :: Char -> Tile
readTile c
  | c == ' ' = Empty
  | c == 'X' = Wall
  | c == 'S' = Start
  | c == 'F' = Finish

main :: IO ()
main = do
  [mazeFile] <- getArgs
  (maze, numRows, numCols, startPos) <- loadMaze mazeFile
  let mp = floodfill maze numRows numCols startPos (Just [])
  print mp

floodfill :: Maze -> Int -> Int -> Pos -> Maybe Path -> Maybe Path
floodfill maze numRows numCols currPos@(i, j) (Just path)
  | i < 0 || i >= numRows || j < 0 || j >= numCols      = Nothing
  | maze ! currPos == Visited || maze ! currPos == Wall = Nothing
  | maze ! currPos == Finish                            = Just $ path ++ [currPos]
  | otherwise = 
    let 
      adjustedMaze = Map.adjust (const Visited) currPos maze
      updatedPath = (Just $ path ++ [currPos])
      right = floodfill adjustedMaze numRows numCols (i + 1, j) updatedPath
      left  = floodfill adjustedMaze numRows numCols (i - 1, j) updatedPath
      up    = floodfill adjustedMaze numRows numCols (i, j + 1) updatedPath
      down  = floodfill adjustedMaze numRows numCols (i, j - 1) updatedPath
    in
      case (down, right, left, up) of
        (Just succPath, _, _, _) -> Just succPath
        (_, Just succPath, _, _) -> Just succPath
        (_, _, Just succPath, _) -> Just succPath
        (_, _, _, Just succPath) -> Just succPath
        _                        -> Nothing
