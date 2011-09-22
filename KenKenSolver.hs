import Data.List
import Data.Maybe
import Control.Monad

import Board

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
	if null newContents 
		then Nothing
		else if length newContents == 1
			then if not $ isResolvedBoard myCells position
				then resolveCell myCells position (head newContents)
				--if It is resolved then we don't need to change
				else Just myCells
			else Just $ insertCell myCells position (UnresolvedCell newContents)


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
	    rows = delete col [1..size]
	    cols = delete row [1..size]
	    boardWithNewCell = insertCell oldCells (row,col) (ResolvedCell targetNum)
	in  foldM (removeNumInRow row) boardWithNewCell cols >>=
		(liftM transpose)
		.(\newBoard -> foldM (removeNumInRow col) newBoard rows)
		.transpose
	where removeNumInRow = (\curRow cells curCol -> 
				removeNumber cells (curRow, curCol) targetNum)
--Currently not used.
operationToFunction :: Operation -> (Int -> Int -> Int)
operationToFunction Add = (+)
operationToFunction Multiply = (*)
operationToFunction Subtract = (-)
operationToFunction Divide = div

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

