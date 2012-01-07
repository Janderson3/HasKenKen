import Data.List
import Data.Maybe
import Control.Monad
import Debug.Trace

import Board
import Bipartite

cellInequalityHelper :: (CellPos, Int) -> [(CellPos,Int)] -> [(Int,Int)] 
cellInequalityHelper source compareList =
	let row = fst $ fst source
	    col = snd $ fst source
	    dex = snd source
	in
	concat $ map (\x -> if sharesRowOrColumn (row,col) $ fst x
			then [(dex, snd x)]
			else [])
		compareList

--Returns pairs of which cells cannot share values since they
--share either a row or column
cellInequality :: [(CellPos, Int)] -> [(Int,Int)]
cellInequality (x:[]) = []
cellInequality (x:xs) = (cellInequalityHelper x xs) ++ (cellInequality xs)

removeNumber :: [[Cell]] -> (Int, Int) ->Int -> Maybe [[Cell]]
removeNumber myCells (row, col) removeNum =
	case targetCell of 
		ResolvedCell curNum -> 
			if curNum == removeNum
				then Nothing
				else Just myCells
		UnresolvedCell cellContents -> 
			let newContents = delete removeNum cellContents
			in updateNewContents myCells (row, col) newContents
	where targetCell = myCells !! (row - 1) !! (col - 1)

updateNewContents :: [[Cell]] -> (Int, Int) -> [Int] -> Maybe [[Cell]]
updateNewContents myCells position newContents = 
	if null updateContents 
		then Nothing
		else if length updateContents == 1
			then if not $ isResolvedBoard myCells position
				then resolveCell myCells position 
					(head updateContents)
				--if It is resolved then we don't need to change
				else Just myCells
			else Just $ insertCell myCells position 
				(UnresolvedCell  updateContents)
	where updateContents = newContents `intersect` (cellContents myCells position)

cellContents :: [[Cell]] -> (Int, Int) -> [Int]
cellContents board (row,col) =
	contentsFromCell $ board !! (row - 1) !! (col - 1)


insertCell :: [[Cell]] -> CellPos -> Cell -> [[Cell]]
insertCell oldCells (row, col) newCell =
	let (before, targetRow:after)  = splitAt (row - 1) oldCells
	    (cellsBefore, _:cellsAfter) = splitAt (col - 1) targetRow
	    newRow = cellsBefore ++ newCell:cellsAfter
	in before ++ newRow:after

--Called when a cell at CellPos has only one element left and thus
--it must be that number.
resolveCell :: [[Cell]] -> CellPos -> Int -> Maybe [[Cell]]
resolveCell oldCells (row,col) targetNum =
	let size = length oldCells
	    rows = delete row [1..size]
	    cols = delete col [1..size]
	    boardWithNewCell = insertCell oldCells (row,col) 
	    				(ResolvedCell targetNum)
	in  foldM (removeNumInRow row) boardWithNewCell cols >>=
		(liftM transpose)
		.(\newBoard -> foldM (removeNumInRow col) newBoard rows)
		.transpose
	where removeNumInRow = (\curRow cells curCol -> removeNumber cells (curRow, curCol) targetNum)
--Currently not used.
{--
operationToFunction :: Operation -> (Int -> Int -> Int)
operationToFunction Add = (+)
operationToFunction Multiply = (*)
operationToFunction Subtract = (-)
operationToFunction Divide = div
--}

--Takes in lists of lists of numbers, returns the lists of numbers
--which satisfy the given restriction 
validPartitions :: [[Int]] -> Restriction -> [[Int]]
validPartitions possibleCombos myRestriction =
	let targetOperation = function myRestriction
	    targetResult = result myRestriction
	in case targetOperation of 
	    	Add -> filter (sumsToNumber targetResult) possibleCombos
		Multiply -> filter (productToNumber targetResult) possibleCombos
		Divide -> filter (dividesToNumber targetResult) possibleCombos
		Subtract -> filter (subtractsToNumber targetResult) possibleCombos

enforceInequalities :: [[Int]] -> Restriction -> [[Int]]
enforceInequalities possibleCombos myRestriction =
	filter (not.violateIneq) possibleCombos
	where violateIneq = (\xs -> any (flip pairAtIndiciesEqual xs)  inequalityList )
	      inequalityList = inequalities myRestriction

pairAtIndiciesEqual :: (Int,Int) -> [Int] -> Bool
pairAtIndiciesEqual (a,b) target =
	(target !! (a - 1)) == (target !! (b - 1))

sumsToNumber :: Int -> [Int] -> Bool
sumsToNumber targetNum daList = sum daList == targetNum

productToNumber :: Int -> [Int] -> Bool
productToNumber targetNum daList = product daList == targetNum

dividesToNumber :: Int -> [Int] -> Bool
dividesToNumber  targetNum (a:b:[])= 
	let greater = max a b
	    lesser = min a b
	in  (greater `mod` lesser == 0) 
		&& (greater `quot` lesser == targetNum)

subtractsToNumber :: Int -> [Int] -> Bool
subtractsToNumber targetNum (a:b:[])= 
	let greater = max a b
	    lesser = min a b
	in  greater - lesser == targetNum

inverseListSequence :: (Eq a) => [[a]] -> [[a]]
inverseListSequence inList = 
	map nub $ foldl unzipListToLists [] inList

--X is the list we're unzipping to and runing down, we're taking
--Y bit by bit and prepending them to each list in X
unzipListToLists :: [[a]] -> [a] -> [[a]]	
unzipListToLists x [] = x --works even if x is null
unzipListToLists [] (y:ys) = [y]:(unzipListToLists [] ys)
unzipListToLists (x:xs) (y:ys) = (y:x):(unzipListToLists xs ys)

refinePartitionsInBoard :: Board -> Maybe Board
refinePartitionsInBoard myBoard =
	let listORegions = regions myBoard
		in foldM refinePartitionInRegion myBoard listORegions
	      
refinePartitionInRegion :: Board -> Region -> Maybe Board
refinePartitionInRegion myBoard myRegion =
	let curCells = cellIndices myRegion	
	    orderedContents = fetchCellsContents myBoard curCells
	    possibleCombos = sequence orderedContents
	    curRestriction = restriction myRegion
	    newContents = refinePartitionInList curRestriction possibleCombos
	    in  liftM  (setContents myBoard) $
	     	  foldM (\runningBoard 
	    	       (curPos, inputContents) 
		       	  -> updateNewContents runningBoard curPos inputContents)
	    	       (contents myBoard)
		       (scrambleList $ zip curCells newContents)
 	    where scrambleList = sortBy (\(_,xs) (_,ys) -> compare  (length ys) (length xs))

refinePartitionInList :: Restriction -> [[Int]] -> [[Int]] 
refinePartitionInList restrc partitions = 
		inverseListSequence $ validPartitions
				(enforceInequalities partitions restrc)
				restrc

updateRow :: [[Cell]] -> [[Int]] -> Int -> Maybe [[Cell]]
updateRow oldBoard newRow rowNum =
	foldM (\runningBoard (contents, index) -> 
		updateNewContents runningBoard (rowNum, index) contents)
			oldBoard $
			zip newRow [1,2..]
			
cellsToContents  :: [[Cell]] -> [[[Int]]]
cellsToContents inBoard =
	map (map contentsFromCell) inBoard

zipIndex :: [a] -> [(Int, a)]
zipIndex = (zip [1..])

perfectMatchFilter :: [[Cell]] -> Maybe [[Cell]]
perfectMatchFilter inBoard =
	filterItoRows inBoard >>=
	justTranspose >>=
	filterItoRows >>=
	justTranspose
	where updateRowDex = (\board (row,contents) -> 
				updateRow board contents row)
	      dexedItoRows = (\board -> (zipIndex $
	      			map itosAlgorithm $ cellsToContents board))
	      filterItoRows = (\board -> foldM updateRowDex 
	      				board $ dexedItoRows board)
justTranspose :: [[a]] -> Maybe [[a]]
justTranspose list = Just $ transpose list

logicStep :: Board -> Maybe Board
logicStep inBoard =
	refinePartitionsInBoard inBoard >>=
	(\x -> perfectMatchFilter $ contents x) >>=
	(\y -> return (setContents inBoard y)) 

solveBoard :: Board -> Maybe Board
solveBoard mahBoard =
	let nextBoard = logicStep mahBoard
	in case nextBoard
		of Nothing -> Nothing
		   Just aBoard -> 
		   	if boardResolved aBoard
				then Just aBoard
				else if (aBoard == mahBoard)
					then boardDFS aBoard
					else logicStep aBoard >>= solveBoard

boardDFS :: Board -> Maybe Board
boardDFS board = Nothing

testRefine :: Maybe Board 
testRefine =
	let myB = testBoard
	in refinePartitionsInBoard  myB
