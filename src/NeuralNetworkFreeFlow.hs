{-# LANGUAGE FlexibleInstances #-}

module NeuralNetworkFreeFlow(simulate) where

import Data.Sequence
import NeuralNetworkGraph

--TYPEDEFS
type Rounds = Int
type Evaporation = Double
type EvaporationPercent = Double

--FUNCTIONS

--support functions
--This function calculates the remaining free energy in the system.
getFreeEnergy :: Graph -> NodeWeight
getFreeEnergy (G nodes _) = sum [ weight | i <- [0..Data.Sequence.length nodes-1], (N _ _ weight) <- [(index nodes i)]]


--preparation functions
--This function creates a Node, with modified weight.
feedNode :: Node -> NodeWeight -> Node
feedNode (N id name weight) plusweight = N id name (weight + plusweight)

--This function makes a feeding step in the graph.
feedGraphHidden :: Graph -> Int -> [NodeWeight] -> Graph
feedGraphHidden (G nodes edges) i modificationWeights 
    | i == 0    = (G (update 0 (feedNode (index nodes 0) (modificationWeights !! 0)) nodes) edges)
    | otherwise = feedGraphHidden (G (update i (feedNode (index nodes i) (modificationWeights !! i)) nodes) edges) (i-1) modificationWeights

--This function feeds the graphs.
--You have to create a NodeWeight list. Length must be equals with the count of the nodes in the graph.
feedGraph :: Graph -> [NodeWeight] -> Graph
feedGraph (G nodes edges) modificationWeights 
    | Data.Sequence.length nodes /= Prelude.length modificationWeights = (G nodes edges)
    | otherwise                                                = feedGraphHidden (G nodes edges) (Data.Sequence.length nodes) modificationWeights

--simulation functions
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
        (E i1 i2 _) = (index edges i)
        (srcNode, modWeight) = runSourceNode (index nodes i1) (index edges i)
        targetNode = runTargetNode (index nodes i2) modWeight

--This function keep alive the algorithm as long as there's free energy in the system.
runSimulation :: Graph -> NodeWeight -> Graph
runSimulation (G nodes edges) remainingFreeWeights
    | remainingFreeWeights > 0 = runSimulation (runSimulationHidden (G nodes edges) (Data.Sequence.length edges-1)) (getFreeEnergy (G nodes edges))
    | otherwise                = G nodes edges

--This function simulates the free flow algorithm with basic evaporation.
simulate :: Graph -> [NodeWeight] -> Graph
simulate (G nodes edges) weightList
    | sum weightList < 0 = G nodes edges
    | otherwise           = runSimulation (feedGraph (G nodes edges) weightList) (sum weightList)
    
    
--UNIT TEST PART

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

--g3 :: Graph
--g3 = runSimulation g2

--g4 :: Graph
--g4 = simulate g2 600

-- simulate g (Succ One)
-- putStr (showGraph (feedGraph g [0.0, 44.0, 0.0]))
