# Rosalind Problem: Implement the Soft k-Means Clustering Algorithm

## Problem Description
The soft k-means clustering algorithm is a variant of k-means where each data point belongs to each cluster with a probability rather than being assigned to a single cluster. The algorithm iteratively updates cluster centers and membership probabilities.

## Solution in Haskell

```haskell
import Data.List (foldl')
import Data.Vector (Vector, (!), (//), fromList, toList)
import qualified Data.Vector as V
import Data.Function (on)
import Data.Ord (comparing)

-- Type aliases for clarity
type Point = Vector Double
type Cluster = Vector Double
type Membership = Vector Double

-- Calculate Euclidean distance between two points
euclideanDistance :: Point -> Point -> Double
euclideanDistance p1 p2 = sqrt $ sum $ zipWith (\x y -> (x - y) ** 2) (toList p1) (toList p2)

-- Calculate squared Euclidean distance
squaredDistance :: Point -> Point -> Double
squaredDistance p1 p2 = sum $ zipWith (\x y -> (x - y) ** 2) (toList p1) (toList p2)

-- Initialize cluster centers randomly
initializeCenters :: Int -> [Point] -> [Cluster]
initializeCenters k points = take k points

-- Calculate membership probabilities for each point to each cluster
-- Using soft k-means with beta parameter (inverse temperature)
calculateMembership :: Double -> [Point] -> [Cluster] -> [[Double]]
calculateMembership beta points centers = map (calculatePointMembership beta centers) points
  where
    calculatePointMembership beta cs point = 
      let distances = map (squaredDistance point) cs
          -- Apply softmax with temperature parameter beta
          expDistances = map (\d -> exp (-beta * d)) distances
          sumExp = sum expDistances
      in map (/ sumExp) expDistances

-- Update cluster centers based on membership probabilities
updateCenters :: Double -> [Point] -> [[Double]] -> [Cluster]
updateCenters beta points memberships = 
  let k = length (head memberships)
      n = length points
      -- For each cluster
      updatedCenters = [updateClusterCenter k n points memberships j | j <- [0..k-1]]
  in updatedCenters
  where
    updateClusterCenter k n points memberships j = 
      let weightedSum = foldl' (\acc pointIdx -> 
        let membership = memberships !! pointIdx !! j
            point = points !! pointIdx
        in V.zipWith (+) acc (V.map (* membership) point)
        ) (V.replicate (V.length (points !! 0)) 0) [0..n-1]
          totalWeight = sum $ map (!! j) memberships
      in if totalWeight == 0 
         then V.replicate (V.length (points !! 0)) 0
         else V.map (/ totalWeight) weightedSum

-- Soft k-means clustering algorithm
softKMeans :: Int -> Double -> Int -> [Point] -> [Cluster]
softKMeans k beta maxIter points = 
  let centers = initializeCenters k points
      finalCenters = iterate (updateCenters' beta points) centers !! maxIter
  in finalCenters
  where
    updateCenters' beta points cs = 
      let memberships = calculateMembership beta points cs
          newCenters = updateCenters beta points memberships
      in newCenters

-- Main function to solve the problem
solveSoftKMeans :: Int -> Double -> Int -> [Point] -> [Cluster]
solveSoftKMeans k beta maxIter points = softKMeans k beta maxIter points

-- Helper function to create Point from list of doubles
pointFromList :: [Double] -> Point
pointFromList xs = fromList xs

-- Helper function to create list of Points from list of lists
pointsFromLists :: [[Double]] -> [Point]
pointsFromLists = map pointFromList

-- Example usage function
exampleUsage :: IO ()
exampleUsage = do
  -- Example data points
  let dataPoints = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0], [7.0, 8.0]]
      points = pointsFromLists dataPoints
      k = 2
      beta = 1.0
      maxIter = 100
      
  putStrLn "Input points:"
  mapM_ print dataPoints
  
  let result = solveSoftKMeans k beta maxIter points
  putStrLn "\nFinal cluster centers:"
  mapM_ print $ map toList result

-- Alternative implementation with explicit iteration
softKMeansIterative :: Int -> Double -> Int -> [Point] -> [Cluster]
softKMeansIterative k beta maxIter points = 
  let initialCenters = initializeCenters k points
      finalCenters = foldl' updateStep initialCenters [1..maxIter]
  in finalCenters
  where
    updateStep centers _ = 
      let memberships = calculateMembership beta points centers
          newCenters = updateCenters beta points memberships
      in newCenters

-- Complete solution for Rosalind problem
-- Input format: k (number of clusters), beta (temperature parameter), 
--                number of iterations, points
softKMeansComplete :: Int -> Double -> Int -> [[Double]] -> [[Double]]
softKMeansComplete k beta maxIter pointData = 
  let points = pointsFromLists pointData
      centers = softKMeans k beta maxIter points
  in map toList centers

-- Test with sample data
testSoftKMeans :: IO ()
testSoftKMeans = do
  -- Sample test case
  let samplePoints = [[0.0, 0.0], [1.0, 0.0], [0.0, 1.0], [1.0, 1.0]]
      k = 2
      beta = 1.0
      maxIter = 10
      
  putStrLn "Testing soft k-means clustering:"
  putStrLn $ "Points: " ++ show samplePoints
  putStrLn $ "k = " ++ show k ++ ", beta = " ++ show beta ++ ", iterations = " ++ show maxIter
  
  let result = softKMeansComplete k beta maxIter samplePoints
  putStrLn "Final cluster centers:"
  mapM_ print result
```

## Key Components Explained

1. **Data Types**: 
   - `Point` and `Cluster` are represented as `Vector Double`
   - `Membership` represents the probability of each point belonging to each cluster

2. **Core Functions**:
   - `euclideanDistance`: Computes distance between two points
   - `calculateMembership`: Computes soft membership probabilities using softmax
   - `updateCenters`: Updates cluster centers based on weighted averages
   - `softKMeans`: Main clustering algorithm

3. **Soft k-Means Logic**:
   - Uses temperature parameter `beta` to control softness
   - Membership probabilities are computed using softmax function
   - Cluster centers are updated using weighted averages

4. **Algorithm Steps**:
   - Initialize cluster centers
   - For each iteration:
     - Calculate membership probabilities for all points
     - Update cluster centers based on memberships
   - Return final cluster centers

## Usage

The function `softKMeansComplete` takes the parameters required by the Rosalind problem and returns the final cluster centers as a list of lists of doubles.

```haskell
-- Example call:
let result = softKMeansComplete 2 1.0 100 [[0.0, 0.0], [1.0, 0.0], [0.0, 1.0], [1.0, 1.0]]
```

This implementation handles the soft k-means clustering algorithm efficiently using Haskell's functional programming features and vector operations.

