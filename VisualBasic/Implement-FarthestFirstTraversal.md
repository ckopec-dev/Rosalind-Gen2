# Rosalind Problem: Implement_FarthestFirstTraversal

## Problem Description
Implement the FarthestFirstTraversal clustering heuristic. Given a set of points in d-dimensional space and a number k, we want to select k centers such that the minimum distance from any point to its nearest center is maximized.

## Solution in Visual Basic

```vb
Module FarthestFirstTraversal
    Sub Main()
        ' Example usage
        Dim points As List(Of List(Of Double)) = New List(Of List(Of Double)) From {
            New List(Of Double) From {0.0, 0.0},
            New List(Of Double) From {1.0, 0.0},
            New List(Of Double) From {0.0, 1.0},
            New List(Of Double) From {1.0, 1.0},
            New List(Of Double) From {2.0, 2.0}
        }
        
        Dim k As Integer = 3
        Dim centers As List(Of List(Of Double)) = FarthestFirstTraversal(points, k)
        
        ' Print results
        For Each center In centers
            Console.WriteLine(String.Join(" ", center.Select(Function(x) x.ToString("F3"))))
        Next
    End Sub
    
    Function FarthestFirstTraversal(points As List(Of List(Of Double)), k As Integer) As List(Of List(Of Double))
        Dim centers As New List(Of List(Of Double))
        
        ' Choose first center randomly (or first point)
        centers.Add(New List(Of Double)(points(0)))
        
        ' Continue until we have k centers
        While centers.Count < k
            Dim maxDistance As Double = -1
            Dim farthestPoint As List(Of Double) = Nothing
            
            ' For each point, find the minimum distance to existing centers
            For Each point In points
                Dim minDistance As Double = Double.MaxValue
                
                ' Find minimum distance to all current centers
                For Each center In centers
                    Dim distance As Double = EuclideanDistance(point, center)
                    If distance < minDistance Then
                        minDistance = distance
                    End If
                Next
                
                ' If this point is farther from its nearest center than any previous point
                If minDistance > maxDistance Then
                    maxDistance = minDistance
                    farthestPoint = point
                End If
            Next
            
            ' Add the farthest point as new center
            centers.Add(New List(Of Double)(farthestPoint))
        End While
        
        Return centers
    End Function
    
    Function EuclideanDistance(point1 As List(Of Double), point2 As List(Of Double)) As Double
        Dim sum As Double = 0
        For i As Integer = 0 To point1.Count - 1
            Dim diff As Double = point1(i) - point2(i)
            sum += diff * diff
        Next
        Return Math.Sqrt(sum)
    End Function
End Module
```

## Algorithm Explanation

1. **Initialization**: Start by selecting the first center (we use the first point in this implementation).

2. **Iterative Selection**: 
   - For each remaining point, calculate its minimum distance to all currently selected centers
   - Among all points, select the one with the maximum minimum distance
   - Add this point as the next center

3. **Repeat**: Continue until we have selected k centers.

## Time Complexity
- O(n²k) where n is the number of points and k is the number of centers
- For each iteration, we need to compute distances to all points and all existing centers

## Space Complexity
- O(k × d) where k is the number of centers and d is the dimensionality of points

## Key Features
- Uses Euclidean distance metric
- Implements the greedy farthest-first traversal heuristic
- Handles multi-dimensional points
- Returns centers in the order they were selected

The algorithm ensures that the minimum distance from any point to its nearest center is maximized, which is the key property of the farthest-first traversal clustering approach.

