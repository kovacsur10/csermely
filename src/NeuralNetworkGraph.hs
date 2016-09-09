{-# LANGUAGE FlexibleInstances #-}

module NeuralNetworkGraph (writeGraphToFile, NodeWeight, EdgeWeight, Rounds, Evaporation, EvaporationPercent, Node, Edge, Nodes, Edges, Graph, feedGraph, simulate, createNode, createEdge, createGraph, gmlPrint) where

import Data.Sequence
import System.IO
import qualified Text.PrettyPrint.HughesPJClass as Pretty

--typedefs
type Identifier = Int
type NodeWeight = Double
type EdgeWeight = Double
type Label = String
type NodeIndex = Int
type Rounds = Int
type Evaporation = Double
type EvaporationPercent = Double

data Node = N Identifier Label NodeWeight
    deriving (Eq,Ord,Show)
data Edge = E NodeIndex NodeIndex EdgeWeight
    deriving (Eq,Ord,Show)
type Nodes = Seq Node
type Edges = Seq Edge
data Graph = G Nodes Edges
    deriving (Eq,Ord,Show)

--data structer functions
createNode :: Identifier -> Label -> NodeWeight -> Node
createNode identifier name weight = N identifier name weight

createEdge :: NodeIndex -> NodeIndex -> EdgeWeight -> Edge
createEdge a b weight
    | a == 0 || b == 0 = E 0 0 0.0
    | otherwise        = E a b weight
    
createGraph :: [Node] -> [Edge] -> Graph
createGraph [] _ = G empty empty
createGraph nodeList edgeList = G (fromList nodeList) (fromList edgeList)

--functions
feed :: Node -> NodeWeight -> Node
feed (N id name weight) w = N id name (weight + w)

modifyNodeWeight :: Node -> NodeWeight -> Node
modifyNodeWeight (N id name weight) plusweight = N id name (weight + plusweight)

feedGraphHidden :: Graph -> Int -> [NodeWeight] -> Graph
feedGraphHidden (G nodes edges) i modify 
    | i == 0    = (G (update 0 (modifyNodeWeight (index nodes 0) (modify !! 0)) nodes) edges)
    | otherwise = feedGraphHidden (G (update i (modifyNodeWeight (index nodes i) (modify !! i)) nodes) edges) (i-1) modify

feedGraph :: Graph -> [NodeWeight] -> Graph
feedGraph (G nodes edges) modify = feedGraphHidden (G nodes edges) (Data.Sequence.length nodes) modify

runSourceNode :: Node -> Edge -> (Node, NodeWeight)
runSourceNode (N id name nodeweight) (E n1 n2 edgeweight)
    | nodeweight-edgeweight <= 0 = (N id name 0.0, nodeweight)
    | otherwise                  = (N id name (nodeweight-edgeweight), edgeweight)

runTargetNode :: Node -> NodeWeight -> Node
runTargetNode (N id name nodeweight) weight = N id name (nodeweight+weight)
    
runSimulationHidden :: Graph -> Int -> Graph
runSimulationHidden (G nodes edges) i 
    | i == 0    = (G (update i1 srcNode (update i2 targetNode nodes)) edges)
    | otherwise = runSimulationHidden (G (update i1 srcNode (update i2 targetNode nodes)) edges) (i-1) where
        (E i1 i2 _) = index edges i
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
    hPutStr fileHandle (gmlPrint gr)
    hClose fileHandle

--pretty printing

gmlNode :: Node -> String
gmlNode (N identifier label weight) = "  node\n  [\n   id " ++ show identifier ++ "\n   label \"Node " ++ id label ++ "\"\n  ]"

gmlEdge :: Edge -> String
gmlEdge (E from to weight) = "  edge\n  [\n   source " ++ show from ++ "\n   target " ++ show to ++ "\n   label \"Edge " ++ show from ++ " to " ++ show to ++ "\"\n  ]"

gmlNodes :: Nodes -> String
gmlNodes nodes = concat [gmlNode (index nodes i) ++ "\n" | i <- [0..(Data.Sequence.length nodes)-1]]

gmlEdges :: Edges -> String
gmlEdges edges = concat [gmlEdge (index edges i) ++ "\n" | i <- [0..(Data.Sequence.length edges)-1]]

gmlPrint :: Graph -> String --frontend should use e.g. putStrLn $ prettyPrint
gmlPrint (G nodes edges) = "creator \"LINK Group HSNetworkSim created by Mate Kovacs, 2016\"\ngraph\n[\n" ++ gmlNodes nodes ++ "\n" ++ gmlEdges edges ++ "]"
    
--variables
nodes :: Seq Node
nodes = empty |> N 0 "1" 0.6 |> N 1 "2" 0.2 |> N 2 "3" 1.0

edges :: Seq Edge
edges = empty |> E 0 1 1.0 |> E 0 2 0.8 |> E 1 2 0.2

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