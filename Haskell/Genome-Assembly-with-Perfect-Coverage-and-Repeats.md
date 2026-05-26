# Rosalind Problem: Genome Assembly with Perfect Coverage and Repeats

## Problem Understanding

This problem involves genome assembly from reads with perfect coverage and repeats. Given a set of k-mers (reads) that perfectly cover a genome, we need to reconstruct the original genome sequence.

## Solution Approach

The key insight is that we're dealing with a de Bruijn graph where:
- Each k-mer becomes a node
- Overlapping k-mers form edges
- The genome is the Eulerian path through this graph

## Haskell Implementation

```haskell
import Data.List (sort, group)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Type aliases for clarity
type Kmer = String
type Graph = Map.Map String [String]

-- Build de Bruijn graph from k-mers
buildDeBruijnGraph :: [Kmer] -> Graph
buildDeBruijnGraph kmers = 
    let edges = [(prefix k, suffix k) | k <- kmers, length k > 0]
    in Map.fromListWith (++) [(u, [v]) | (u, v) <- edges]

-- Extract prefix and suffix of a k-mer
prefix :: Kmer -> Kmer
prefix k = take (length k - 1) k

suffix :: Kmer -> Kmer
suffix k = drop 1 k

-- Find Eulerian path using Hierholzer's algorithm
findEulerianPath :: Graph -> Maybe String
findEulerianPath graph = 
    let startNode = findStartNode graph
        path = eulerianPath startNode graph []
    in if isValidPath path graph 
       then Just path
       else Nothing

-- Find a starting node (node with more outgoing than incoming edges)
findStartNode :: Graph -> String
findStartNode graph = 
    let nodes = Map.keys graph
        inDegree = Map.fromListWith (+) [(node, 1) | node <- concatMap snd (Map.elems graph)]
        outDegree = Map.fromListWith (+) [(node, 1) | node <- nodes, node <- Map.findWithDefault [] node graph]
        diff = [(node, (fromMaybe 0 (Map.lookup node outDegree) - fromMaybe 0 (Map.lookup node inDegree))) | node <- nodes]
        startNodes = [node | (node, deg) <- diff, deg > 0]
    in if null startNodes 
       then head nodes 
       else head startNodes

-- Recursive Eulerian path finding
eulerianPath :: String -> Graph -> String -> String
eulerianPath current graph path
    | null edges = current ++ path
    | otherwise = 
        let next = head edges
            newGraph = Map.insertWith (++) current (tail edges) graph
        in eulerianPath next newGraph (current ++ path)
  where edges = Map.findWithDefault [] current graph

-- Alternative approach: build the genome from k-mers directly
assembleGenome :: [Kmer] -> String
assembleGenome kmers = 
    let k = length (head kmers)
        graph = buildDeBruijnGraph kmers
        start = head kmers
        path = findEulerianPath graph
    in case path of
        Just p -> p
        Nothing -> 
            -- Fallback: simple overlap assembly
            let overlaps = buildOverlapMap kmers
                assembled = assembleFromOverlaps overlaps k
            in assembled

-- Build overlap map for simple assembly
buildOverlapMap :: [Kmer] -> [(String, String)]
buildOverlapMap kmers = 
    [(k1, k2) | k1 <- kmers, k2 <- kmers, k1 /= k2, suffix k1 == prefix k2]

-- Simple assembly from overlaps
assembleFromOverlaps :: [(String, String)] -> Int -> String
assembleFromOverlaps overlaps k = 
    let start = head [k1 | (k1, k2) <- overlaps, not (k2 `elem` map snd overlaps)]
        assembled = assembleHelper start overlaps []
    in assembled

assembleHelper :: String -> [(String, String)] -> String -> String
assembleHelper current overlaps path
    | null next = current ++ path
    | otherwise = 
        let (k1, k2) = head next
            newCurrent = k2
            newPath = current ++ path
        in assembleHelper newCurrent (filter (/= (k1, k2)) overlaps) newPath
  where next = [(k1, k2) | (k1, k2) <- overlaps, k1 == current]

-- More robust approach using De Bruijn graph construction
deBruijnAssembly :: [Kmer] -> String
deBruijnAssembly kmers = 
    let k = length (head kmers)
        nodes = map prefix kmers ++ map suffix kmers
        uniqueNodes = map head (group (sort nodes))
        edges = [(prefix kmer, suffix kmer) | kmer <- kmers]
        graph = Map.fromListWith (++) [(u, [v]) | (u, v) <- edges]
        eulerian = findEulerianPath graph
    in case eulerian of
        Just path -> path
        Nothing -> 
            -- If no Eulerian path, try to construct manually
            let first = head kmers
                last = last kmers
                assembled = first ++ drop 1 last
            in assembled

-- Main function for the problem
solveGenomeAssembly :: [Kmer] -> String
solveGenomeAssembly kmers = 
    if null kmers
    then ""
    else 
        let k = length (head kmers)
            -- Build the de Bruijn graph
            graph = buildDeBruijnGraph kmers
            -- Try to find Eulerian path
            path = findEulerianPath graph
        in case path of
            Just p -> p
            Nothing -> 
                -- Fallback approach
                let startKmer = head kmers
                    assembled = foldl (\acc kmer -> acc ++ drop 1 kmer) startKmer (tail kmers)
                in assembled

-- Example usage
main :: IO ()
main = do
    -- Example input
    let reads = ["AAAT", "AATG", "ATGG", "TGGT", "GGTA", "GTA"]
    let result = solveGenomeAssembly reads
    putStrLn $ "Assembled genome: " ++ result
```

## Key Points

1. **De Bruijn Graph**: Each k-mer is represented as a node, and overlaps form edges
2. **Eulerian Path**: The genome sequence corresponds to an Eulerian path through the graph
3. **Edge Cases**: Handle cases where perfect coverage might not form a simple Eulerian cycle
4. **Overlap Detection**: Use prefix/suffix matching to build the graph

## Time Complexity
- Building the graph: O(n × k) where n is number of k-mers and k is k-mer length
- Finding Eulerian path: O(n + m) where m is number of edges
- Overall: O(n × k)

## Space Complexity
- O(n × k) for storing the graph

This solution handles perfect coverage with repeats by leveraging the properties of de Bruijn graphs and Eulerian paths to reconstruct the original genome sequence.

