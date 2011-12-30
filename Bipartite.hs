import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Query.DFS
import Debug.Trace
import Data.Tuple


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

{- Takes a list like bipartiteFromList and returns all the nodes
which can be in a perfect matching -}
itosAlgorithm :: [[Int]] -> [[Int]]
itosAlgorithm inList = 
	map assembleData [1,3.. (2 * length inList)]
	where  sccs = partitionEdges inList
	       assembleData position = map (`div`2) $ 
			filter even $ 
			head $ filter (elem position) sccs

{- Returns the sccs of Ito's algorithm-}
partitionEdges :: [[Int]] -> [[Int]]
partitionEdges  inList = 
	scc . insBackEdges . hopcroftKarp $ bipartiteFromList inList

{- Takes in a graph hopcroft karp spits out and puts in
"back edges" The edges that go from V -> U will have
edges that go from U ->V -}
insBackEdges :: DynGraph gr => gr a b -> gr a b
insBackEdges gra =
	let v = filter even $ nodes gra
	    pres = map head $ filter (not.null) $ map (\x -> suc gra x) v
	    newEdges =  zip pres v
	in foldl (\gra2 ver@(vert1,vert2) 
		-> insEdge (vert1,vert2,edgeLabel gra $ swap ver)
		gra2) 
		gra
		newEdges

hopcroftKarp :: DynGraph gr => gr a b -> gr a b
hopcroftKarp gra =
	case hopcroftStep gra
	of Nothing -> gra
	   Just gra2 -> hopcroftKarp gra2
	   

{- This function takes a bipartite graph where the odd nodes are
one set in the bipartite graph, and the even nodes are another also
all unmatched edges go from odd to even and all matched edges are 
the ones that travel in the reverse. the function returns nothing if
the input graph is optimal-}

	--I'm going to use the set names that wikipedia uses

hopcroftStep :: DynGraph gr => gr a b -> Maybe (gr a b)
hopcroftStep aGraph = 
	let u = filter odd $ nodes aGraph
	    v = filter even $ nodes aGraph
	    ufree = filter (\x -> 0 == indeg aGraph x) u
	    vfree = filter (\x -> 0 == outdeg aGraph x) v
	    levelsVfree = filter (\(node,_) -> node `elem` vfree)
			 $ leveln (zip ufree $ repeat 0) aGraph
	    f = foldl assembleF  [] levelsVfree
	    ps = assemblePaths aGraph (snd $ head f) ufree (map fst f)
	in if null ps
	   then
		Nothing
	   else
		Just $ flipPaths aGraph ps


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

{- Takes in a graph how far to go, sources and sinks-}
assemblePaths :: Graph gr => 
	gr a b -> Int -> [Node] -> [Node] ->[[Node]]
assemblePaths _ _ [] _ = []
assemblePaths _ _ _ [] = []
assemblePaths gra count (v:vs) sinks =
	case guidedDFS gra count v sinks
	of
		(Nothing,_) -> assemblePaths gra count vs sinks
		(Just something, rgra) ->  something : assemblePaths rgra count vs sinks

flipPaths :: DynGraph gr => gr a b -> [[Node]] -> gr a b		
flipPaths gra paths = foldl flipPath gra paths

flipPath :: DynGraph gr => gr a b -> [Node] -> gr a b 
flipPath gra (fin:[]) = gra
flipPath gra (source:sink:rest) = 
	flipPath (flipEdge gra (source,sink)) (sink:rest)
	

flipEdge :: DynGraph gr => gr a b -> (Node, Node) -> gr a b
flipEdge gra edge@(source, sink) = insEdge (sink,source,(edgeLabel gra edge)) 
	$ delEdge edge gra

edgeLabel :: Graph gr => gr a b -> (Node, Node) -> b
edgeLabel gra (x,y) =
	thd3.head $ filter (\(src,snk,lab) -> src == x && snk == y) 
		$ labEdges gra
		where thd3 (_,_,c) = c

--Assume all the things that come in are free and elements of
--V
assembleF ::  [(Node,Int)] -> (Node, Int) -> [(Node,Int)]
assembleF [] b = [b]
assembleF s@((node,depth):xs)  (nodeIn,depthIn)  
	| depth > depthIn = [(nodeIn,depthIn)]
	| depth == depthIn = (nodeIn,depthIn):s
	| otherwise = s
