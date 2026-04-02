# Rosalind Problem: Implement_FarthestFirstTraversal

## Problem Description
Implement the FarthestFirstTraversal clustering algorithm. Given a set of points in d-dimensional space and a number k, we want to select k points as centers such that the minimum distance from any point to its nearest center is maximized.

## Solution in Haskell

```haskell
import Data.List (foldl')
import Data.Ord (comparing)
import Data.Array (Array, listArray, (!))
import Data.Ix (range)

-- Point type representing a d-dimensional point
type Point = [Double]

-- Distance function (Euclidean distance)
distance :: Point -> Point -> Double
distance p1 p2 = sqrt $ sum $ map (\(x,y) -> (x-y)^2) $ zip p1 p2

-- Find the minimum distance from a point to any center
minDistance :: Point -> [Point] -> Double
minDistance point centers = minimum $ map (distance point) centers

-- FarthestFirstTraversal algorithm
farthestFirstTraversal :: Int -> [Point] -> [Point]
farthestFirstTraversal k points
  | k <= 0 = []
  | length points < k = points
  | otherwise = go [firstPoint] (filter (\p -> p /= firstPoint) points)
  where
    -- Choose the first point (can be any point, here we use the first one)
    firstPoint = head points
    
    -- Main traversal loop
    go :: [Point] -> [Point] -> [Point]
    go centers remaining
      | length centers >= k = centers
      | otherwise = 
          let nextPoint = farthestPoint remaining centers
          in go (centers ++ [nextPoint]) (filter (/= nextPoint) remaining)

-- Find the point in remaining that is farthest from all centers
farthestPoint :: [Point] -> [Point] -> Point
farthestPoint remaining centers = 
  fst $ foldl' updateBest (head remaining, 0) remaining
  where
    updateBest (bestPoint, bestMinDist) currentPoint
      | currentMinDist > bestMinDist = (currentPoint, currentMinDist)
      | otherwise = (bestPoint, bestMinDist)
      where
        currentMinDist = minDistance currentPoint centers

-- Alternative implementation with explicit index tracking
farthestFirstTraversal' :: Int -> [Point] -> [Point]
farthestFirstTraversal' k points
  | k <= 0 = []
  | length points < k = points
  | otherwise = 
      let firstCenter = head points
          remainingPoints = filter (/= firstCenter) points
      in firstCenter : go [firstCenter] remainingPoints
  where
    go :: [Point] -> [Point] -> [Point]
    go centers [] = centers
    go centers remaining = 
      let nextCenter = findFarthestCenter remaining centers
      in go (centers ++ [nextCenter]) (filter (/= nextCenter) remaining)

-- Find the point that maximizes the minimum distance to existing centers
findFarthestCenter :: [Point] -> [Point] -> Point
findFarthestCenter points centers = 
  maximumBy (comparing (minDistance centers)) points
  where
    maximumBy :: (a -> a -> Ordering) -> [a] -> a
    maximumBy cmp [] = error "Empty list"
    maximumBy cmp (x:xs) = foldl' (\acc y -> if cmp acc y == GT then acc else y) x xs

-- More efficient version using foldl'
farthestFirstTraversal'' :: Int -> [Point] -> [Point]
farthestFirstTraversal'' k points
  | k <= 0 = []
  | length points < k = points
  | otherwise = 
      let firstPoint = head points
          remainingPoints = filter (/= firstPoint) points
      in firstPoint : go [firstPoint] remainingPoints
  where
    go :: [Point] -> [Point] -> [Point]
    go centers [] = centers
    go centers remaining = 
      let (nextCenter, _) = foldl' updateBest (head remaining, -1) remaining
          nextCenterList = filter (/= nextCenter) remaining
      in nextCenter : go (centers ++ [nextCenter]) nextCenterList
      where
        updateBest (bestPoint, bestDist) currentPoint
          | currentMinDist > bestDist = (currentPoint, currentMinDist)
          | otherwise = (bestPoint, bestDist)
          where
            currentMinDist = minimum $ map (distance currentPoint) centers

-- Final clean implementation
farthestFirstTraversal :: Int -> [Point] -> [Point]
farthestFirstTraversal k points
  | k <= 0 = []
  | length points < k = points
  | otherwise = 
      let firstPoint = head points
          remainingPoints = filter (/= firstPoint) points
      in firstPoint : go [firstPoint] remainingPoints
  where
    go :: [Point] -> [Point] -> [Point]
    go centers [] = centers
    go centers remaining = 
      let nextCenter = maximumBy (comparing (minDistance centers)) remaining
          nextRemaining = filter (/= nextCenter) remaining
      in nextCenter : go (centers ++ [nextCenter]) nextRemaining

-- Helper function to compare using a key function
maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ [] = error "Empty list"
maximumBy cmp (x:xs) = foldl' (\acc y -> if cmp acc y == GT then acc else y) x xs

-- Example usage
example :: IO ()
example = do
  let points = [[0,0], [1,1], [2,2], [3,3], [4,4]]
      k = 3
  print $ farthestFirstTraversal k points
```

## Key Components

1. **Point Type**: `[Double]` representing d-dimensional points
2. **Distance Function**: Computes Euclidean distance between two points
3. **minDistance**: Finds minimum distance from a point to existing centers
4. **farthestFirstTraversal**: Main algorithm that selects k centers
5. **farthestPoint**: Finds the point that maximizes minimum distance to existing centers

## Algorithm Steps

1. Start with any point as the first center
2. For each subsequent center:
   - Find the point in the remaining points that has the maximum minimum distance to all existing centers
   - Add this point as the next center
3. Repeat until k centers are selected

## Time Complexity
- O(n²k) where n is the number of points and k is the number of centers
- Each iteration requires checking all remaining points against all existing centers

## Space Complexity
- O(k) for storing the centers
- O(n) for the remaining points during computation

