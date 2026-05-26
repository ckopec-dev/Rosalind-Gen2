# Rosalind Problem: Distances_in_Trees (Haskell Solution)

## Problem Understanding

The problem asks us to compute the distance matrix for a tree given its adjacency list representation. The distance between any two nodes is the number of edges in the unique path connecting them.

## Approach

1. Parse the input to build an adjacency list representation of the tree
2. For each pair of nodes, compute the shortest path distance using BFS
3. Output the distance matrix

## Solution

```haskell
import Data.List
import Data.Maybe
import qualified Data.Map as Map

-- Parse input and build adjacency list
buildGraph :: [String] -> Map.Map Int [Int]
buildGraph lines = 
    let edges = map parseEdge lines
        nodes = nub $ concatMap (\(a,b) -> [a,b]) edges
        graph = Map.fromList [(n, []) | n <- nodes]
    in foldl addEdge graph edges
  where
    parseEdge :: String -> (Int, Int)
    parseEdge line = 
        let [a,b] = map read $ words line :: [Int]
        in (a,b)
    
    addEdge :: Map.Map Int [Int] -> (Int, Int) -> Map.Map Int [Int]
    addEdge g (a,b) = 
        let neighborsA = Map.findWithDefault [] a g
            neighborsB = Map.findWithDefault [] b g
        in Map.insert a (b:neighborsA) $ Map.insert b (a:neighborsB) g

-- BFS to find shortest path distance
bfsDistance :: Map.Map Int [Int] -> Int -> Int -> Int
bfsDistance graph start end
    | start == end = 0
    | otherwise = 
        let visited = Map.singleton start True
        in bfsHelper graph visited [start] 0
  where
    bfsHelper :: Map.Map Int [Int] -> Map.Map Int Bool -> [Int] -> Int -> Int
    bfsHelper g visited [] dist = -1  -- Not found
    bfsHelper g visited (current:queue) dist
        | current == end = dist
        | otherwise = 
            let neighbors = Map.findWithDefault [] current g
                unvisitedNeighbors = filter (\n -> not $ Map.member n visited) neighbors
                newVisited = foldl (\acc n -> Map.insert n True acc) visited unvisitedNeighbors
                newQueue = queue ++ unvisitedNeighbors
            in bfsHelper g newVisited newQueue (dist + 1)

-- Generate distance matrix
distanceMatrix :: [String] -> [[Int]]
distanceMatrix input = 
    let lines = filter (not . null) input
        graph = buildGraph lines
        nodes = Map.keys graph
        n = length nodes
        matrix = [[bfsDistance graph i j | j <- nodes] | i <- nodes]
    in matrix

-- Main function to solve the problem
solve :: [String] -> [String]
solve input = 
    let matrix = distanceMatrix input
        formatted = map (unwords . map show) matrix
    in formatted

-- Example usage
main :: IO ()
main = do
    -- Example input (you would read from file in practice)
    let input = [
            "1 2",
            "2 3",
            "3 4",
            "4 5",
            "5 6"
        ]
    mapM_ putStrLn $ solve input
```

## Alternative Implementation (More Efficient)

```haskell
import qualified Data.Map as Map
import Data.List (nub)
import Data.Queue (Queue, empty, enqueue, dequeue, isEmpty)

-- More efficient approach using BFS for all pairs
distanceMatrixEfficient :: [String] -> [[Int]]
distanceMatrixEfficient input = 
    let lines = filter (not . null) input
        graph = buildGraph lines
        nodes = Map.keys graph
        n = length nodes
        matrix = [[shortestPathDistance graph i j | j <- nodes] | i <- nodes]
    in matrix

-- Build adjacency list representation
buildGraph :: [String] -> Map.Map Int [Int]
buildGraph lines = 
    let edges = map parseEdge lines
        allNodes = nub $ concatMap (\(a,b) -> [a,b]) edges
        graph = Map.fromList [(n, []) | n <- allNodes]
    in foldl addEdge graph edges
  where
    parseEdge :: String -> (Int, Int)
    parseEdge line = 
        let [a,b] = map read $ words line :: [Int]
        in (a,b)
    
    addEdge :: Map.Map Int [Int] -> (Int, Int) -> Map.Map Int [Int]
    addEdge g (a,b) = 
        let neighborsA = Map.findWithDefault [] a g
            neighborsB = Map.findWithDefault [] b g
        in Map.insert a (b:neighborsA) $ Map.insert b (a:neighborsB) g

-- BFS to find shortest path distance between two nodes
shortestPathDistance :: Map.Map Int [Int] -> Int -> Int -> Int
shortestPathDistance graph start end
    | start == end = 0
    | otherwise = 
        let visited = Map.singleton start True
        in bfs (Map.singleton start 0) visited [start]
  where
    bfs :: Map.Map Int Int -> Map.Map Int Bool -> [Int] -> Int
    bfs distances visited [] = -1  -- Not found
    bfs distances visited (current:queue)
        | current == end = distances Map.! current
        | otherwise = 
            let neighbors = Map.findWithDefault [] current graph
                unvisitedNeighbors = filter (\n -> not $ Map.member n visited) neighbors
                newDistances = foldl updateDistance distances unvisitedNeighbors
                newVisited = foldl (\acc n -> Map.insert n True acc) visited unvisitedNeighbors
                newQueue = queue ++ unvisitedNeighbors
            in bfs newDistances newVisited newQueue
      where
        updateDistance :: Map.Map Int Int -> Int -> Map.Map Int Int
        updateDistance dists node = 
            let currentDist = distances Map.! current
            in Map.insert node (currentDist + 1) dists

-- Format output
formatOutput :: [[Int]] -> [String]
formatOutput matrix = map (unwords . map show) matrix

-- Main function
solveDistancesInTrees :: [String] -> [String]
solveDistancesInTrees input = 
    let matrix = distanceMatrixEfficient input
    in formatOutput matrix
```

## Key Points

1. **Graph Representation**: Using `Data.Map` for efficient adjacency list storage
2. **BFS Algorithm**: For finding shortest path distances in unweighted trees
3. **Input Parsing**: Simple parsing of edge list format
4. **Distance Matrix**: Computing all pairwise distances between nodes

## Time Complexity
- Building graph: O(E) where E is number of edges
- Computing all distances: O(N × (N + E)) where N is number of nodes
- Overall: O(N × (N + E))

This solution handles the tree distance computation efficiently and correctly outputs the required distance matrix.

