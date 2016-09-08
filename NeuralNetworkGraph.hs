{-# LANGUAGE FlexibleInstances #-}

module NeuralNetworkGraph (writeGraphToFile, NodeWeight, EdgeWeight, Rounds, Evaporation, EvaporationPercent, Node, Edge, Nodes, Edges, Graph, feedGraph, showGraph, simulate, createNode, createEdge, createGraph) where

import Data.Sequence
import System.IO

--typedefs
type NodeWeight = Double
type EdgeWeight = Double
type Rounds = Int
type Evaporation = Double
type EvaporationPercent = Double

newtype Node = N (String, NodeWeight)
	deriving (Eq,Ord)
newtype Edge = E (Int, Int, EdgeWeight)
	deriving (Eq,Ord)
type Nodes = Seq Node
type Edges = Seq Edge
data Graph = G Nodes Edges
	deriving (Eq,Ord)
	
instance Show Node
	where
		show (N (name, weight)) = " Node " ++ show name ++ " " ++ show weight ++ " "
		
instance Show Edge
	where
		show (E (node1, node2, weight)) = "Edge (" ++ show node1 ++ ")--" ++ show weight ++ "--(" ++ show node2 ++ ")\n"
	
instance Show Graph
	where
		show (G nodes edges) = "Graph\nNodes:\n" ++ show nodes ++ "\nEdges:\n" ++ show edges

--data structer functions
createNode :: String -> NodeWeight -> Node
createNode name weight = N (name, weight)

createEdge :: Int -> Int -> EdgeWeight -> Edge
createEdge a b weight
	| a == 0 || b == 0 = E (0,0,0.0)
	| otherwise        = E (a, b, weight)
	
createGraph :: [Node] -> [Edge] -> Graph
createGraph [] _ = G empty empty
createGraph nodeList edgeList = G (fromList nodeList) (fromList edgeList)

--functions
feed :: Node -> NodeWeight -> Node
feed (N n) w = N (fst n, snd n + w)

modifyNodeWeight :: Node -> NodeWeight -> Node
modifyNodeWeight (N (name, weight)) plusweight = N (name, weight + plusweight)

feedGraphHidden :: Graph -> Int -> [NodeWeight] -> Graph
feedGraphHidden (G nodes edges) i modify 
	| i == 0    = (G (update 0 (modifyNodeWeight (index nodes 0) (modify !! 0)) nodes) edges)
	| otherwise = feedGraphHidden (G (update i (modifyNodeWeight (index nodes i) (modify !! i)) nodes) edges) (i-1) modify

feedGraph :: Graph -> [NodeWeight] -> Graph
feedGraph (G nodes edges) modify = feedGraphHidden (G nodes edges) (Data.Sequence.length nodes) modify

showNodes :: Nodes -> String
showNodes nodes = concat ["Node " ++ (show name) ++ ": " ++ (show weight) ++ "\n" | i <- [0..(Data.Sequence.length nodes-1)], N (name, weight) <- [index nodes i]]

showEdges :: Graph -> String
showEdges (G nodes edges) = concat ["Edge (" ++ (show (index nodes n1)) ++ ")--" ++ show w ++ "--(" ++ (show (index nodes n2)) ++ ")\n" | i <- [0..(Data.Sequence.length edges-1)], E (n1, n2, w) <- [index edges i]]

showGraph :: Graph -> String
showGraph (G nodes edges) = "Graph\nNodes:\n" ++ showNodes nodes ++ "\nEdges:\n" ++ showEdges (G nodes edges)

runSourceNode :: Node -> Edge -> (Node, NodeWeight)
runSourceNode (N (name, nodeweight)) (E (n1,n2,edgeweight))
	| nodeweight-edgeweight <= 0 = (N (name,0.0), nodeweight)
	| otherwise 				 = (N (name, nodeweight-edgeweight), edgeweight)

runTargetNode :: Node -> NodeWeight -> Node
runTargetNode (N (name, nodeweight)) weight = N (name, nodeweight+weight)
	
runSimulationHidden :: Graph -> Int -> Graph
runSimulationHidden (G nodes edges) i 
	| i == 0    = (G (update i1 srcNode (update i2 targetNode nodes)) edges)
	| otherwise = runSimulationHidden (G (update i1 srcNode (update i2 targetNode nodes)) edges) (i-1) where
		(E (i1, i2, _)) = (index edges i)
		(srcNode, modWeight) = runSourceNode (index nodes i1) (index edges i)
		targetNode = runTargetNode (index nodes i2) modWeight

runSimulation :: Graph -> Graph
runSimulation (G nodes edges) = runSimulationHidden (G nodes edges) (Data.Sequence.length edges-1)
	
simulate :: Graph -> Rounds -> Graph
simulate (G nodes edges) r
	| r <= 0    = (G nodes edges)
	| otherwise = runSimulation (simulate (G nodes edges) (r-1))

writeGraphToFile :: Graph -> String -> IO ()
writeGraphToFile gr fileName = do
	fileHandle <- openFile fileName WriteMode
	hPutStr fileHandle (showGraph gr)
	hClose fileHandle

--variables
nodes :: Seq Node
nodes = empty |> N ("1", 0.6) |> N ("2", 0.2) |> N ("3", 1.0) 

edges :: Seq Edge
edges = empty |> E (0, 1, 1.0) |> E (0, 2, 0.8) |> E (1, 2, 0.2)

--          0.6
--        /     \
--      1.0      0.8
--      v          v
--   0.2  --0.2--> 1.0

g :: Graph
g = G nodes edges

g2 :: Graph
g2 = feedGraph g [68.0, 44.0, 23.0]

g3 :: Graph
g3 = runSimulation g2

g4 :: Graph
g4 = simulate g2 600

-- simulate g (Succ One)
-- putStr (showGraph (feedGraph g [0.0, 44.0, 0.0]))