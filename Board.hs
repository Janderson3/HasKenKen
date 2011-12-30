
module Board
( Board(contents, regions, size)
, Operation(Add,Multiply,Subtract,Divide)
, Region(cellIndices, restriction)
, cellInit
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
, fetchCellsContents 
, testBoard
, contentsFromCell
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

contentsFromCell :: Cell -> [Int]
contentsFromCell (UnresolvedCell cont) = cont
contentsFromCell (ResolvedCell cont) = [cont]

setContents :: Board -> [[Cell]] -> Board
setContents oldBoard newContents = 
	Board {contents = newContents, regions = regions oldBoard, 
		size = size oldBoard}

fetchCellsContents :: Board -> [CellPos] -> [[Int]]
fetchCellsContents myBoard =
	(map (getCellContents myBoard)) 

testBoard :: Board
testBoard = 
  let bContents = contentsInit 4
      bRegions = testRegions
  in Board bContents bRegions 4
  where testRegions = 
		[ Region [(1,1), (1,2)] $ Restriction Subtract 2 [(1,2)]
		, Region [(1,3), (1,4)] $ Restriction Divide 2 [(1,2)]
		, Region [(2,1), (3,1)] $ Restriction Subtract 3 [(1,2)]
		, Region [(2,2), (2,3)] $ Restriction Multiply 6 [(1,2)]
		, Region [(2,4), (3,4)] $ Restriction Divide 2 [(1,2)]
		, Region [(3,2), (3,3), (4,3)] $ Restriction Add 8 [(1,2), (2,3)]
		, Region [(4,1), (4,2)] $ Restriction Divide 2 [(1,2)]
		, Region [(4,4)] $ Restriction Add 3 []
		]
