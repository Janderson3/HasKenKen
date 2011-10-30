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

testGraph :: Gr [Char] ()
testGraph =(mkGraph 
	[(1,"Left 1"),(2,"Right 1"),(3,"Left 2"),(4,"Right 2")]
	[(1,2,()),(1,4,()),(3,2,()),(3,4,())])

{- This function takes a bipartite graph where the odd nodes are
one set in the bipartite graph, and the even nodes are another also
all unmatched edges go from odd to even and all matched edges are 
the ones that travel in the reverse. -}

{-
hopcroftStep :: Graph gr => gr a b -> Maybe gr a b
hopcroftStep aGraph = 
	--I'm going to use the set names that wikipedia uses
	let 
		U = filter odd $ nodes aGraph
		V = filter even $ nodes aGraph
		Ufree = filter (\x -> 0 == $ indeg x aGraph) U
		Vfree = filter (\x -> 0 == $ outdeg x aGraph) V
		levelsVfree = filter (\(node,_) -> node `elem` Vfree)
			 $ leveln (zip Ufree $ repeat 0) aGraph
		F = foldl assembleF  [] levelsVfree
-}


--Finds a path from a target node to any number of sink nodes
--but only if that path is less than a certain length
--Then it returns a tupple of the found path 
-- and the rest of the graph
guidedDFS :: Graph gr => 
	gr a b -> Int -> Node -> [Node] -> (Maybe [Node], gr a b)
guidedDFS gra 0 source sinks =
	if source `elem` sinks
		then (Just [source], delNode source gra)
		--The graph won't matter because we won't use it
		else (Nothing, gra)
guidedDFS gra count source sinks =
	let
		newGraph = snd $ match source gra
		nextNodes = suc gra source
	in case guidedDFSHelper newGraph count nextNodes sinks
	   of (Nothing, _) -> (Nothing, gra)
	      (Just path, restGra) -> (Just $ source:path, restGra)

guidedDFSHelper :: Graph gr =>
	gr a b -> Int -> [Node] -> [Node] -> (Maybe [Node], gr a b)
guidedDFSHelper gra _ [] sinks = (Nothing, gra)
guidedDFSHelper gra count (x:xs) sinks =
	case guidedDFS gra (count - 1) x sinks 
	of
		(Nothing,_) -> guidedDFSHelper gra count xs sinks
		something -> something



--Assume all the things that come in are free and elements of
--V
assembleF ::  [(Node,Int)] -> (Node, Int) -> [(Node,Int)]
assembleF [] b = [b]
assembleF s@((node,depth):xs)  (nodeIn,depthIn)  
	| depth > depthIn = [(nodeIn,depthIn)]
	| depth == depthIn = (nodeIn,depthIn):s
	| otherwise = s
