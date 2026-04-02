# Bellman-Ford Algorithm Solution in Haskell

## Problem Understanding

The Bellman-Ford algorithm finds the shortest paths from a source vertex to all other vertices in a weighted graph, even when negative edge weights are present. It can also detect negative cycles.

## Solution

```haskell
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

-- Graph representation: adjacency list with edge weights
type Graph = Map.Map Int [(Int, Int)]  -- vertex -> [(neighbor, weight)]

-- Bellman-Ford algorithm implementation
bellmanFord :: Graph -> Int -> Either String [Int]
bellmanFord graph source = 
    let vertices = Map.keys graph
        n = length vertices
        -- Initialize distances
        distances = Map.fromList [(v, if v == source then 0 else 1000000) | v <- vertices]
    in if n == 0
       then Left "Empty graph"
       else 
           let result = bellmanFordIter graph distances source n
           in if hasNegativeCycle graph result
              then Left "Negative cycle detected"
              else Right (map (fromMaybe 1000000) (map (`Map.lookup` result) vertices))

-- Main Bellman-Ford iteration
bellmanFordIter :: Graph -> Map.Map Int Int -> Int -> Int -> Map.Map Int Int
bellmanFordIter graph distances source iterations
    | iterations <= 0 = distances
    | otherwise = 
        let newDistances = foldl' updateDistances distances (Map.toList graph)
        in bellmanFordIter graph newDistances source (iterations - 1)

-- Update distances for all edges
updateDistances :: Map.Map Int Int -> (Int, [(Int, Int)]) -> Map.Map Int Int
updateDistances distances (u, edges) = 
    foldl' (updateFromEdge distances) distances edges
  where
    updateFromEdge dists (v, weight) = 
        let distU = fromMaybe 1000000 (Map.lookup u dists)
            distV = fromMaybe 1000000 (Map.lookup v dists)
            newDist = distU + weight
        in if newDist < distV 
           then Map.insert v newDist dists 
           else dists

-- Check for negative cycles
hasNegativeCycle :: Graph -> Map.Map Int Int -> Bool
hasNegativeCycle graph distances = 
    any (\(u, edges) -> 
        any (\(v, weight) -> 
            let distU = fromMaybe 1000000 (Map.lookup u distances)
                distV = fromMaybe 1000000 (Map.lookup v distances)
            in distU + weight < distV) edges) 
        (Map.toList graph)

-- Parse input from Rosalind format
parseInput :: String -> (Graph, Int)
parseInput input = 
    let lines = map (map read . words) (lines input)
        [n, m] = head lines
        edges = tail lines
        source = 1  -- Assuming source is vertex 1
        graph = foldl' addEdge Map.empty edges
    in (graph, source)
  where
    addEdge graph (u, v, w) = 
        let edgesU = fromMaybe [] (Map.lookup u graph)
        in Map.insert u ((v, w) : edgesU) graph

-- Main function to solve the problem
solveRosalind :: String -> Either String [Int]
solveRosalind input = 
    let (graph, source) = parseInput input
    in bellmanFord graph source

-- Example usage
exampleInput :: String
exampleInput = "6 10\n1 2 8\n1 3 5\n1 4 2\n2 4 2\n2 5 4\n3 4 3\n3 6 3\n4 5 1\n5 6 1\n4 6 4"

-- Test the solution
main :: IO ()
main = do
    let result = solveRosalind exampleInput
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right distances -> putStrLn $ "Distances: " ++ show distances
```

## Alternative Cleaner Implementation

```haskell
import qualified Data.Map as Map
import Data.List (foldl')

-- Simplified Bellman-Ford implementation
bellmanFordSimple :: [(Int, Int, Int)] -> Int -> Int -> Either String [Int]
bellmanFordSimple edges source nVertices = 
    let -- Initialize distances
        distances = Map.fromList [(v, if v == source then 0 else 1000000) | v <- [1..nVertices]]
        -- Relax edges repeatedly
        finalDistances = foldl' relax distances [1..nVertices-1]
        -- Check for negative cycles
        hasNegCycle = any (\(u, v, w) -> 
            let distU = fromMaybe 1000000 (Map.lookup u finalDistances)
                distV = fromMaybe 1000000 (Map.lookup v finalDistances)
            in distU + w < distV) edges
    in if hasNegCycle
       then Left "Negative cycle detected"
       else Right [fromMaybe 1000000 (Map.lookup v finalDistances) | v <- [1..nVertices]]
  where
    relax dists (u, v, w) = 
        let distU = fromMaybe 1000000 (Map.lookup u dists)
            distV = fromMaybe 1000000 (Map.lookup v dists)
            newDist = distU + w
        in if newDist < distV 
           then Map.insert v newDist dists 
           else dists

-- Parse Rosalind input format
parseRosalindInput :: String -> ([(Int, Int, Int)], Int, Int)
parseRosalindInput input = 
    let lines = map (map read . words) (lines input)
        [nVertices, nEdges] = head lines
        edges = tail lines
    in (map (\[u, v, w] -> (u, v, w)) edges, nVertices, nEdges)

-- Complete solution
solveBellmanFord :: String -> Either String [Int]
solveBellmanFord input = 
    let (edges, n, _) = parseRosalindInput input
    in bellmanFordSimple edges 1 n
```

## Key Features of the Solution

1. **Graph Representation**: Uses `Data.Map` for efficient vertex lookups
2. **Negative Cycle Detection**: Checks for negative cycles after relaxation
3. **Error Handling**: Returns `Either String [Int]` to handle error cases
4. **Rosalind Format**: Parses input in the standard Rosalind format
5. **Efficient Implementation**: Uses `foldl'` for strict evaluation and better performance

## Time Complexity
- **Time**: O(|V| × |E|) where V is vertices and E is edges
- **Space**: O(|V|) for storing distances

## Usage
The solution can be used to solve the Rosalind Bellman-Ford problem by parsing the input format and applying the Bellman-Ford algorithm to find shortest paths from a source vertex to all other vertices.

