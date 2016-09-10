{-# LANGUAGE FlexibleInstances #-}

module NeuralNetworkGraph (writeGraphToFile, NodeWeight, EdgeWeight, Node(..), Edge(..), Nodes, Edges, Graph(..), createNode, createEdge, createGraph) where

import Data.Sequence
import System.IO

--TYPEDEFS
type Label = String
type Identifier = Int
type NodeWeight = Double
type EdgeWeight = Double

data Node = N Identifier Label NodeWeight
    deriving (Eq,Ord, Show)
data Edge = E Identifier Identifier EdgeWeight
    deriving (Eq,Ord, Show)
type Nodes = Seq Node
type Edges = Seq Edge
data Graph = G Nodes Edges
    deriving (Eq,Ord, Show)

--FUNCTIONS
--data structer functions
createNode :: Identifier -> Label -> NodeWeight -> Node
createNode identifier name weight = N identifier name weight

createEdge :: Identifier -> Identifier -> EdgeWeight -> Edge
createEdge a b weight
    | a == 0 || b == 0 = E 0 0 0.0
    | otherwise        = E a b weight

createGraph :: [Node] -> [Edge] -> Graph
createGraph [] _ = G empty empty
createGraph nodeList edgeList = G (fromList nodeList) (fromList edgeList)

--i/o functions
writeGraphToFile :: Graph -> String -> IO ()
writeGraphToFile gr fileName = do
    fileHandle <- openFile fileName WriteMode
    hPutStr fileHandle (gmlPrint gr)
    hClose fileHandle

--pretty printing

gmlNode :: Node -> String
gmlNode (N identifier label weight) = "  node\n  [\n   id " ++ show identifier ++ "\n   label \"Node " ++ id label ++ "\"\n   weight " ++ show weight ++ "\n  ]"

gmlEdge :: Edge -> String
gmlEdge (E from to weight) = "  edge\n  [\n   source " ++ show from ++ "\n   target " ++ show to ++ "\n   label \"Edge " ++ show from ++ " to " ++ show to ++ "\"\n   weight " ++ show weight ++ "\n  ]"

gmlNodes :: Nodes -> String
gmlNodes nodes = concat [gmlNode (index nodes i) ++ "\n" | i <- [0..(Data.Sequence.length nodes)-1]]

gmlEdges :: Edges -> String
gmlEdges edges = concat [gmlEdge (index edges i) ++ "\n" | i <- [0..(Data.Sequence.length edges)-1]]

gmlPrint :: Graph -> String --frontend should use e.g. putStrLn $ prettyPrint
gmlPrint (G nodes edges) = "creator \"LINK Group HSNetworkSim created by Mate Kovacs, 2016\"\ngraph\n[\n" ++ gmlNodes nodes ++ "\n" ++ gmlEdges edges ++ "]"

--UNIT TEST PART
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