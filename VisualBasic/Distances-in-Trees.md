# Rosalind Problem: Distances_in_Trees (VB.NET Solution)

## Problem Understanding

The problem asks us to compute the distance matrix for a tree given as input. The distance between any two nodes in a tree is the number of edges on the unique path connecting them.

## Solution Approach

1. Parse the input tree structure
2. Build an adjacency list representation of the tree
3. For each pair of nodes, compute the shortest path using BFS
4. Output the distance matrix

## VB.NET Implementation

```vb
Imports System
Imports System.Collections.Generic
Imports System.IO
Imports System.Text

Module DistancesInTrees
    Sub Main()
        ' Read input from file
        Dim inputLines As String() = File.ReadAllLines("rosalind_tree.txt")
        
        ' Parse the tree
        Dim tree As New Tree()
        tree.ParseInput(inputLines)
        
        ' Compute distance matrix
        Dim distances As Integer(,) = tree.ComputeDistanceMatrix()
        
        ' Output result
        For i As Integer = 0 To distances.GetLength(0) - 1
            For j As Integer = 0 To distances.GetLength(1) - 1
                Console.Write(distances(i, j) & " ")
            Next
            Console.WriteLine()
        Next
    End Sub
End Module

Class Tree
    Private adjacencyList As Dictionary(Of Integer, List(Of Integer))
    Private nodes As Integer
    
    Public Sub New()
        adjacencyList = New Dictionary(Of Integer, List(Of Integer))
        nodes = 0
    End Sub
    
    Public Sub ParseInput(inputLines As String())
        ' First line contains number of nodes
        nodes = Integer.Parse(inputLines(0))
        
        ' Initialize adjacency list
        For i As Integer = 1 To nodes
            adjacencyList(i) = New List(Of Integer)()
        Next
        
        ' Process edges
        For i As Integer = 1 To inputLines.Length - 1
            If inputLines(i).Trim() = "" Then Continue For
            
            Dim parts() As String = inputLines(i).Split(" "c)
            Dim node1 As Integer = Integer.Parse(parts(0))
            Dim node2 As Integer = Integer.Parse(parts(1))
            
            ' Add edges in both directions (undirected graph)
            adjacencyList(node1).Add(node2)
            adjacencyList(node2).Add(node1)
        Next
    End Sub
    
    Public Function ComputeDistanceMatrix() As Integer(,)
        Dim matrix(nodes - 1, nodes - 1) As Integer
        
        ' For each node, compute distances to all other nodes
        For i As Integer = 1 To nodes
            Dim distances As Dictionary(Of Integer, Integer) = BFS(i)
            
            For j As Integer = 1 To nodes
                matrix(i - 1, j - 1) = distances(j)
            Next
        Next
        
        Return matrix
    End Function
    
    Private Function BFS(startNode As Integer) As Dictionary(Of Integer, Integer)
        Dim distances As New Dictionary(Of Integer, Integer)()
        Dim queue As New Queue(Of Integer)()
        Dim visited As New HashSet(Of Integer)()
        
        ' Initialize all distances to -1 (unreachable)
        For i As Integer = 1 To nodes
            distances(i) = -1
        Next
        
        ' Start BFS
        queue.Enqueue(startNode)
        distances(startNode) = 0
        visited.Add(startNode)
        
        While queue.Count > 0
            Dim current As Integer = queue.Dequeue()
            
            ' Visit all neighbors
            For Each neighbor As Integer In adjacencyList(current)
                If Not visited.Contains(neighbor) Then
                    visited.Add(neighbor)
                    distances(neighbor) = distances(current) + 1
                    queue.Enqueue(neighbor)
                End If
            Next
        End While
        
        Return distances
    End Function
End Class
```

## Sample Input Format

The input file should contain:
```
4
1 2
1 3
1 4
```

## Sample Output Format

For the above input, the output would be:
```
0 1 1 1 
1 0 2 2 
1 2 0 2 
1 2 2 0 
```

## Key Points

1. **Tree Representation**: Uses adjacency list to represent the undirected tree
2. **Distance Calculation**: For each node, performs BFS to compute distances to all other nodes
3. **Matrix Construction**: Builds a symmetric distance matrix where matrix[i,j] represents the distance between nodes i and j
4. **Time Complexity**: O(n²) where n is the number of nodes
5. **Space Complexity**: O(n²) for storing the distance matrix

## Notes

- The input format assumes the first line contains the number of nodes
- Edges are given as pairs of node numbers
- Node numbering starts from 1 (not 0)
- The solution handles trees with any number of nodes and edges

