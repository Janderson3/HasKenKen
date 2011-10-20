import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree


everyOther :: [a] -> [a] -> [a]
everyOther left = zippedPairToList.zip left

zippedPairToList :: [(a,a)] -> [a]
zippedPairToList [] = []
zippedPairToList ((left,right):xs) = left:right:zippedPairToList xs

makeEdgesFromList :: [[Int]] -> Int ->  [LEdge ()]
makeEdgesFromList [] _ = []
makeEdgesFromList (x:xs) dex = 
	map (\y -> (2*dex - 1, 2*y, ())) x ++ makeEdgesFromList xs (dex + 1)

--Nodes in U(Positions) are odd nodes in V(data) are even
--Why? I said so

bipartiteFromList ::  [[Int]] -> Gr [Char] ()
bipartiteFromList sourceList = 
	let posList = zip [1,3..] (map (\x -> "Pos " ++ (show x)) [1..])
	    datList = zip [2,4..] (map (\x -> "Dat " ++ (show x)) [1..])
	    nodeList = take  (2 * length sourceList) $ 
	    	everyOther posList datList
	    edgeList = makeEdgesFromList sourceList 1
        in mkGraph nodeList edgeList

