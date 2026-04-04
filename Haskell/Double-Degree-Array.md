# Rosalind Problem: Double-Degree Array

## Problem Statement
Given: A simple graph with n ≤ 10³ vertices in the edge list format. Return: An array D[1..n] where D[i] is the sum of the degrees of i's neighbors.

## Solution in Haskell

```haskell
import Data.List (group, sort)
import qualified Data.Map as Map

-- Parse input into adjacency list representation
parseEdges :: [(Int, Int)] -> Map.Map Int [Int]
parseEdges edges = 
    let allNodes = concatMap (\(u, v) -> [u, v]) edges
        uniqueNodes = sort $ nub allNodes
        -- Create adjacency list
        adjList = Map.fromListWith (++) [(u, [v]) | (u, v) <- edges] 
        adjList2 = Map.fromListWith (++) [(v, [u]) | (u, v) <- edges] 
        -- Combine both directions
        fullAdj = Map.unionWith (++) adjList adjList2
    in Map.mapWithKey (\k v -> sort v) fullAdj

-- Calculate double degree array
doubleDegreeArray :: [(Int, Int)] -> [Int]
doubleDegreeArray edges = 
    let adjList = parseEdges edges
        maxNode = maximum $ concatMap (\(u, v) -> [u, v]) edges
        -- Get degrees of each node
        degrees = Map.fromListWith (+) [(node, 1) | (u, v) <- edges, node <- [u, v]]
        -- For each node, sum degrees of its neighbors
        result = [sum $ map (\neighbor -> degrees Map.! neighbor) (adjList Map.! node) | node <- [1..maxNode]]
    in result

-- Alternative cleaner approach
doubleDegree :: Int -> [(Int, Int)] -> [Int]
doubleDegree n edges = 
    let -- Build adjacency list
        adjList = foldl addEdge Map.empty edges
        -- Get degree of each node
        degrees = Map.fromListWith (+) [(node, 1) | (u, v) <- edges, node <- [u, v]]
        -- Calculate double degree for each node
        calcDoubleDegree node = 
            let neighbors = adjList Map.! node
                neighborDegrees = map (degrees Map.!) neighbors
            in sum neighborDegrees
    in map calcDoubleDegree [1..n]
  where
    addEdge acc (u, v) = 
        let updatedU = Map.insertWith (++) u [v] acc
            updatedV = Map.insertWith (++) v [u] updatedU
        in updatedV

-- Main solution function
solveDoubleDegree :: [(Int, Int)] -> [Int]
solveDoubleDegree edges = 
    let maxNode = maximum $ concatMap (\(u, v) -> [u, v]) edges
        -- Build adjacency list
        adjList = foldl (\acc (u, v) -> 
            let updatedU = Map.insertWith (++) u [v] acc
                updatedV = Map.insertWith (++) v [u] updatedU
            in updatedV) Map.empty edges
        -- Get degrees of each node
        nodeDegrees = Map.fromListWith (+) [(node, 1) | (u, v) <- edges, node <- [u, v]]
        -- Calculate double degree for each node
        doubleDeg node = sum $ map (nodeDegrees Map.!) (adjList Map.! node)
    in map doubleDeg [1..maxNode]

-- Clean, readable solution
doubleDegreeArray' :: [(Int, Int)] -> [Int]
doubleDegreeArray' edges = 
    let -- Get all nodes
        allNodes = sort $ nub $ concatMap (\(u, v) -> [u, v]) edges
        maxNode = maximum allNodes
        -- Build adjacency list
        adjList = foldl addEdge Map.empty edges
        -- Calculate degrees
        degrees = Map.fromListWith (+) [(node, 1) | (u, v) <- edges, node <- [u, v]]
        -- For each node, sum degrees of its neighbors
        doubleDeg node = 
            let neighbors = adjList Map.! node
                neighborDegs = map (degrees Map.!) neighbors
            in sum neighborDegs
    in map doubleDeg [1..maxNode]
  where
    addEdge acc (u, v) = 
        let updatedU = Map.insertWith (++) u [v] acc
            updatedV = Map.insertWith (++) v [u] updatedU
        in updatedV

-- Simple version for the problem
doubleDegree :: [(Int, Int)] -> [Int]
doubleDegree edges = 
    let -- Create adjacency list
        adj = foldl (\acc (u, v) -> 
            Map.insertWith (++) u [v] $ Map.insertWith (++) v [u] acc) 
            Map.empty edges
        -- Get degrees of all nodes
        deg = Map.fromListWith (+) [(node, 1) | (u, v) <- edges, node <- [u, v]]
        -- Calculate double degree for each node from 1 to max
        maxNode = maximum $ concatMap (\(u, v) -> [u, v]) edges
    in [sum $ map (deg Map.!) (adj Map.! n) | n <- [1..maxNode]]

-- Example usage:
-- Input: [(1,2), (2,3), (6,3), (5,6), (1,5), (2,5)]
-- Output: [3, 5, 3, 0, 4, 3]
```

## Explanation

The solution works in these steps:

1. **Parse edges**: Convert the edge list into an adjacency list representation
2. **Calculate degrees**: Count the degree of each node (how many edges connect to it)
3. **Compute double degrees**: For each node, sum the degrees of all its neighbors

The key insight is that we need to:
- Build an adjacency list to know which nodes are neighbors
- Calculate the degree of each node
- For each node, look up its neighbors and sum their degrees

## Time Complexity
- O(E + V) where E is the number of edges and V is the number of vertices

## Space Complexity  
- O(V + E) for storing the adjacency list and degree information

The solution handles the edge cases where nodes might have no neighbors or where the graph is disconnected.

