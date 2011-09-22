
module Board
( Board(contents, regions, size)
, Operation(Add,Multiply,Subtract,Divide)
, Region
, cellInit
, restriction
, Restriction(Restriction, function, result, inequalities)
, Cell(ResolvedCell, UnresolvedCell)
, CellPos
, isResolvedCell
, isResolvedBoard
, contentsInit
, sharesRowOrColumn
, zipWithIndex
, getCellContents
, setContents
) where

import Data.List
import Data.Maybe
import Control.Monad

data Board = Board { contents :: [[Cell]]
		   , regions :: [Region]
		   , size :: Int 
		   }

data Operation = Add | Multiply | Subtract | Divide deriving (Eq, Show)

data Region = Region { cellIndices :: [CellPos]
	 	     , restriction :: Restriction
		     }

data Restriction = Restriction { function :: Operation
			       , result :: Int
			       , inequalities :: [(Int,Int)]
			       }	

data Cell = ResolvedCell Int | UnresolvedCell  [Int] deriving (Show)

type CellPos = (Int, Int)

isResolvedCell :: Cell -> Bool
isResolvedCell (ResolvedCell _) = True
isResolvedCell _ = False

isResolvedBoard :: [[Cell]] -> CellPos -> Bool
isResolvedBoard myBoard (row, col) =
	isResolvedCell $ myBoard !! (row - 1) !! (col - 1)


cellInit :: Int -> Cell
cellInit size = UnresolvedCell [1..size]

contentsInit :: Int -> [[Cell]]
contentsInit boardSize = 
	replicate boardSize $ replicate boardSize $ cellInit boardSize

sharesRowOrColumn :: CellPos -> CellPos -> Bool
sharesRowOrColumn (a,b) (c,d) = or [a == c, b == d]
			
zipWithIndex :: [a] -> [(a,Int)]
zipWithIndex inList = zip inList [1..]

getCellContents :: Board -> CellPos -> [Int]
getCellContents myBoard (row, col) =
	-- !! are apparently array indecies and are thus 0 indexed. Who knew?
	let cell = contents myBoard !! (row - 1) !! (col - 1) 
	in case cell of 
		UnresolvedCell cont -> cont
		ResolvedCell cont -> [cont]

setContents :: Board -> [[Cell]] -> Board
setContents oldBoard newContents = 
	Board {contents = newContents, regions = regions oldBoard, 
		size = size oldBoard}

