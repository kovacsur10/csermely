{-# LANGUAGE FlexibleInstances #-}

module NeuralNetworkTest where

import NeuralNetworkGraph

basicGraph :: Graph
basicGraph = createGraph [createNode (show i) (i * 0.265) | i <- [0..20]] [createEdge i j (fromIntegral (i+j) * 0.04) | i <- [0..20], j <- [i..20]]

circle :: Graph
circle = createGraph [createNode "0" 1.0] [createEdge 0 0 1.0] 

g :: Graph
g = simulate basicGraph 4

worm :: Graph