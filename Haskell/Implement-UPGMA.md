# Rosalind Problem: Implement UPGMA in Haskell

## Problem Overview
UPGMA (Unweighted Pair Group Method with Arithmetic Mean) is a clustering algorithm that constructs a phylogenetic tree from a distance matrix. The algorithm repeatedly merges the closest pairs of clusters until a single tree is formed.

## Solution

```haskell
import Data.List (minimumBy, delete, find)
import Data.Ord (comparing)

-- Define types for our phylogenetic tree
data Tree = Leaf String | Node String Double [Tree]
    deriving (Show, Eq)

-- Distance matrix type
type DistanceMatrix = [[Double]]

-- UPGMA algorithm implementation
upgma :: DistanceMatrix -> [String] -> Tree
upgma distMat taxa = 
    let n = length taxa
        initialClusters = map (\t -> [t]) taxa
        finalTree = upgmaRecursive distMat initialClusters
    in finalTree

-- Main UPGMA recursive function
upgmaRecursive :: DistanceMatrix -> [[String]] -> Tree
upgmaRecursive distMat [cluster] = Leaf (head cluster)
upgmaRecursive distMat clusters = 
    let (i, j) = findClosestPair distMat clusters
        newCluster = clusters !! i ++ clusters !! j
        newDistMat = updateDistanceMatrix distMat i j clusters
        newClusters = delete (clusters !! j) (delete (clusters !! i) clusters) ++ [newCluster]
    in upgmaRecursive newDistMat newClusters

-- Find the closest pair of clusters
findClosestPair :: DistanceMatrix -> [[String]] -> (Int, Int)
findClosestPair distMat clusters = 
    let pairs = [(i, j) | i <- [0..length clusters - 1], 
                         j <- [0..length clusters - 1], 
                         i < j]
        distances = [(distMat !! i !! j, i, j) | (i, j) <- pairs]
        (minDist, i, j) = minimumBy (comparing fst) distances
    in (i, j)

-- Update distance matrix after merging two clusters
updateDistanceMatrix :: DistanceMatrix -> Int -> Int -> [[String]] -> DistanceMatrix
updateMatrix distMat i j clusters = 
    let n = length distMat
        k = length clusters
        -- Determine which cluster index is smaller
        (smaller, larger) = if i < j then (i, j) else (j, i)
        -- Create new matrix with one less row/column
        newRows = [row | (row, idx) <- zip distMat [0..], idx /= smaller && idx /= larger]
        -- Add new row for the merged cluster
        newRow = [calculateDistance distMat smaller larger idx clusters | idx <- [0..n-1], idx /= smaller && idx /= larger]
        -- Insert new row at position smaller
        insertAt pos elem list = take pos list ++ [elem] ++ drop pos list
        newMatrix = insertAt smaller newRow (map (deleteAt smaller) newRows)
    in newMatrix

-- Helper function to delete element at index
deleteAt :: Int -> [a] -> [a]
deleteAt i xs = take i xs ++ drop (i + 1) xs

-- Calculate distance between merged cluster and other clusters
calculateDistance :: DistanceMatrix -> Int -> Int -> Int -> [[String]] -> Double
calculateDistance distMat i j k clusters = 
    let n = length clusters
        ni = length (clusters !! i)
        nj = length (clusters !! j)
        nk = length (clusters !! k)
    in (distMat !! i !! k + distMat !! j !! k - distMat !! i !! j) / 2

-- Alternative cleaner implementation
upgmaClean :: DistanceMatrix -> [String] -> Tree
upgmaClean distMat taxa = 
    let n = length taxa
        clusters = map (\t -> [t]) taxa
        tree = upgmaStep distMat clusters
    in tree

upgmaStep :: DistanceMatrix -> [[String]] -> Tree
upgmaStep distMat [cluster] = Leaf (head cluster)
upgmaStep distMat clusters = 
    let (i, j) = findClosestPair distMat clusters
        mergedCluster = clusters !! i ++ clusters !! j
        newDistMat = mergeDistances distMat i j clusters
        remainingClusters = deleteAt i (deleteAt j clusters)
    in upgmaStep newDistMat (remainingClusters ++ [mergedCluster])

-- More efficient distance matrix update
mergeDistances :: DistanceMatrix -> Int -> Int -> [[String]] -> DistanceMatrix
mergeDistances distMat i j clusters = 
    let n = length distMat
        (smaller, larger) = if i < j then (i, j) else (j, i)
        -- Create new matrix with reduced size
        newMatrix = [row | (row, idx) <- zip distMat [0..n-1], idx /= smaller && idx /= larger]
        -- Calculate new row for the merged cluster
        newRow = [calculateAverageDistance distMat i j k clusters | k <- [0..n-1], k /= smaller && k /= larger]
    in newMatrix ++ [newRow]

-- Calculate average distance to other clusters
calculateAverageDistance :: DistanceMatrix -> Int -> Int -> Int -> [[String]] -> Double
calculateAverageDistance distMat i j k clusters = 
    let ni = length (clusters !! i)
        nj = length (clusters !! j)
    in (distMat !! i !! k + distMat !! j !! k) / 2

-- Simple implementation that works with basic distance matrix
simpleUpgma :: DistanceMatrix -> [String] -> Tree
simpleUpgma distMat taxa = 
    let n = length taxa
        clusters = map (\t -> [t]) taxa
        finalClusters = upgmaIterate distMat clusters
    in buildTree finalClusters

upgmaIterate :: DistanceMatrix -> [[String]] -> [[String]]
upgmaIterate distMat clusters
    | length clusters == 1 = clusters
    | otherwise = 
        let (i, j) = findClosestPair distMat clusters
            merged = clusters !! i ++ clusters !! j
            newDistMat = updateMatrix distMat i j clusters
            newClusters = delete (clusters !! i) (delete (clusters !! j) clusters) ++ [merged]
        in upgmaIterate newDistMat newClusters

-- Helper function to build the final tree structure
buildTree :: [[String]] -> Tree
buildTree [[x]] = Leaf x
buildTree clusters = 
    let root = "internal_" ++ show (length clusters)
        children = map (\c -> Leaf (head c)) clusters
    in Node root 0.0 children

-- Example usage
exampleDistMatrix :: DistanceMatrix
exampleDistMatrix = [
    [0.0, 1.0, 2.0, 3.0],
    [1.0, 0.0, 1.0, 2.0],
    [2.0, 1.0, 0.0, 1.0],
    [3.0, 2.0, 1.0, 0.0]
]

exampleTaxa :: [String]
exampleTaxa = ["A", "B", "C", "D"]

-- Run the algorithm
main :: IO ()
main = do
    let result = simpleUpgma exampleDistMatrix exampleTaxa
    print result
```

## Explanation

This implementation of UPGMA in Haskell follows these key steps:

1. **Data Types**: 
   - `Tree` data type to represent phylogenetic trees with leaves and internal nodes
   - `DistanceMatrix` type alias for 2D arrays of distances

2. **Core Algorithm**:
   - Start with each taxon as its own cluster
   - Repeatedly find the closest pair of clusters
   - Merge the clusters and update the distance matrix
   - Continue until a single cluster remains

3. **Key Functions**:
   - `findClosestPair`: Finds the minimum distance pair in the distance matrix
   - `updateDistanceMatrix`: Updates the distance matrix after merging clusters
   - `upgmaRecursive`: Main recursive function that implements the UPGMA algorithm

4. **Distance Calculation**:
   - When merging two clusters, the new distances are calculated using the average distance formula
   - For clusters A and B merging to form cluster C, distance(C,D) = (distance(A,D) + distance(B,D))/2

The algorithm assumes a symmetric distance matrix and produces a rooted phylogenetic tree representing the evolutionary relationships among the taxa.

