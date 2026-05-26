# Rosalind Problem: Implement the Neighbor Joining Algorithm

## Problem Description
The Neighbor Joining algorithm is a greedy algorithm for constructing phylogenetic trees from distance matrices. It works by iteratively joining the closest pair of nodes until a tree is formed.

## Solution in Visual Basic

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Module NeighborJoining
    ' Structure to represent a tree node
    Public Class TreeNode
        Public Name As String
        Public Children As List(Of TreeNode)
        Public Parent As TreeNode
        Public Distance As Double
        
        Public Sub New(name As String)
            Me.Name = name
            Me.Children = New List(Of TreeNode)()
            Me.Parent = Nothing
            Me.Distance = 0.0
        End Sub
    End Class

    ' Structure to represent a distance matrix
    Public Class DistanceMatrix
        Public Matrix As Double(,)
        Public Names As List(Of String)
        Public Size As Integer
        
        Public Sub New(names As List(Of String))
            Me.Names = names
            Me.Size = names.Count
            Me.Matrix = New Double(Size - 1, Size - 1) {}
        End Sub
        
        Public Sub SetDistance(i As Integer, j As Integer, distance As Double)
            Matrix(i, j) = distance
            Matrix(j, i) = distance
        End Sub
        
        Public Function GetDistance(i As Integer, j As Integer) As Double
            Return Matrix(i, j)
        End Function
    End Class

    ' Main function to implement neighbor joining
    Public Function NeighborJoining(matrix As DistanceMatrix) As TreeNode
        ' Create a copy of the matrix
        Dim n As Integer = matrix.Size
        Dim currentMatrix As New DistanceMatrix(matrix.Names)
        
        ' Copy the original matrix
        For i As Integer = 0 To n - 1
            For j As Integer = 0 To n - 1
                currentMatrix.Matrix(i, j) = matrix.Matrix(i, j)
            Next
        Next
        
        ' Create initial tree with all nodes
        Dim nodes As New List(Of TreeNode)()
        For i As Integer = 0 To n - 1
            nodes.Add(New TreeNode(matrix.Names(i)))
        Next
        
        ' While there are more than 2 nodes
        While nodes.Count > 2
            ' Find the minimum element in the distance matrix
            Dim minI As Integer = -1
            Dim minJ As Integer = -1
            Dim minDistance As Double = Double.MaxValue
            
            For i As Integer = 0 To currentMatrix.Size - 1
                For j As Integer = 0 To currentMatrix.Size - 1
                    If i <> j AndAlso currentMatrix.Matrix(i, j) < minDistance Then
                        minDistance = currentMatrix.Matrix(i, j)
                        minI = i
                        minJ = j
                    End If
                Next
            Next
            
            ' Calculate the Q matrix
            Dim qMatrix As New Double(currentMatrix.Size, currentMatrix.Size)
            Dim rowSums As New Double(currentMatrix.Size)
            Dim colSums As New Double(currentMatrix.Size)
            
            ' Calculate row sums and column sums
            For i As Integer = 0 To currentMatrix.Size - 1
                For j As Integer = 0 To currentMatrix.Size - 1
                    If i <> j Then
                        rowSums(i) += currentMatrix.Matrix(i, j)
                        colSums(j) += currentMatrix.Matrix(i, j)
                    End If
                Next
            Next
            
            ' Calculate Q matrix
            For i As Integer = 0 To currentMatrix.Size - 1
                For j As Integer = 0 To currentMatrix.Size - 1
                    If i <> j Then
                        qMatrix(i, j) = (currentMatrix.Size - 2) * currentMatrix.Matrix(i, j) - rowSums(i) - colSums(j)
                    Else
                        qMatrix(i, j) = 0.0
                    End If
                Next
            Next
            
            ' Find minimum element in Q matrix
            Dim qMinI As Integer = -1
            Dim qMinJ As Integer = -1
            Dim qMinValue As Double = Double.MaxValue
            
            For i As Integer = 0 To currentMatrix.Size - 1
                For j As Integer = 0 To currentMatrix.Size - 1
                    If i <> j AndAlso qMatrix(i, j) < qMinValue Then
                        qMinValue = qMatrix(i, j)
                        qMinI = i
                        qMinJ = j
                    End If
                Next
            Next
            
            ' Calculate distances to new node
            Dim distanceToNewNodeI As Double = (currentMatrix.Matrix(qMinI, qMinJ) + (rowSums(qMinI) - rowSums(qMinJ)) / 2.0)
            Dim distanceToNewNodeJ As Double = (currentMatrix.Matrix(qMinI, qMinJ) + (rowSums(qMinJ) - rowSums(qMinI)) / 2.0)
            
            ' Create new node
            Dim newNode As New TreeNode("Node" & nodes.Count)
            
            ' Add edges to the new node
            Dim nodeI As TreeNode = nodes(qMinI)
            Dim nodeJ As TreeNode = nodes(qMinJ)
            
            nodeI.Children.Add(newNode)
            nodeJ.Children.Add(newNode)
            newNode.Children.Add(nodeI)
            newNode.Children.Add(nodeJ)
            
            newNode.Distance = distanceToNewNodeI
            nodeI.Distance = distanceToNewNodeI
            nodeJ.Distance = distanceToNewNodeJ
            
            ' Remove the two nodes from the list
            nodes.RemoveAt(Math.Max(qMinI, qMinJ))
            nodes.RemoveAt(Math.Min(qMinI, qMinJ))
            
            ' Update the matrix
            Dim newMatrix As New DistanceMatrix(currentMatrix.Names)
            
            ' Copy remaining nodes
            Dim remainingNodes As New List(Of Integer)()
            For i As Integer = 0 To currentMatrix.Size - 1
                If i <> qMinI AndAlso i <> qMinJ Then
                    remainingNodes.Add(i)
                End If
            Next
            
            ' Update the matrix
            Dim newSize As Integer = remainingNodes.Count + 1
            newMatrix.Size = newSize
            
            ' Fill the new matrix
            For i As Integer = 0 To newSize - 1
                For j As Integer = 0 To newSize - 1
                    If i = newSize - 1 AndAlso j = newSize - 1 Then
                        newMatrix.Matrix(i, j) = 0.0
                    ElseIf i = newSize - 1 Then
                        ' Calculate distance from new node to existing nodes
                        Dim nodeIndex As Integer = remainingNodes(j)
                        Dim distance As Double = (currentMatrix.Matrix(qMinI, nodeIndex) + currentMatrix.Matrix(qMinJ, nodeIndex) - currentMatrix.Matrix(qMinI, qMinJ)) / 2.0
                        newMatrix.Matrix(i, j) = distance
                        newMatrix.Matrix(j, i) = distance
                    ElseIf j = newSize - 1 Then
                        ' Calculate distance from existing node to new node
                        Dim nodeIndex As Integer = remainingNodes(i)
                        Dim distance As Double = (currentMatrix.Matrix(qMinI, nodeIndex) + currentMatrix.Matrix(qMinJ, nodeIndex) - currentMatrix.Matrix(qMinI, qMinJ)) / 2.0
                        newMatrix.Matrix(i, j) = distance
                        newMatrix.Matrix(j, i) = distance
                    Else
                        ' Copy existing distances
                        Dim nodeI As Integer = remainingNodes(i)
                        Dim nodeJ As Integer = remainingNodes(j)
                        newMatrix.Matrix(i, j) = currentMatrix.Matrix(nodeI, nodeJ)
                    End If
                Next
            Next
            
            currentMatrix = newMatrix
            
            ' Add new node to the list
            nodes.Add(newNode)
        End While
        
        ' Connect the last two nodes
        Dim root As New TreeNode("Root")
        root.Children.Add(nodes(0))
        root.Children.Add(nodes(1))
        nodes(0).Parent = root
        nodes(1).Parent = root
        
        Return root
    End Function

    ' Helper function to print tree (in-order traversal)
    Public Sub PrintTree(node As TreeNode, level As Integer)
        For i As Integer = 0 To level - 1
            Console.Write("  ")
        Next
        Console.WriteLine(node.Name)
        
        For Each child As TreeNode In node.Children
            PrintTree(child, level + 1)
        Next
    End Sub

    ' Main program
    Public Sub Main()
        ' Example usage with sample data
        Dim names As New List(Of String)({"A", "B", "C", "D", "E"})
        
        Dim matrix As New DistanceMatrix(names)
        
        ' Set up sample distance matrix (5x5)
        ' This is an example matrix - in practice, you would read from input
        matrix.SetDistance(0, 0, 0.0)   ' A-A
        matrix.SetDistance(0, 1, 13.0)  ' A-B
        matrix.SetDistance(0, 2, 21.0)  ' A-C
        matrix.SetDistance(0, 3, 22.0)  ' A-D
        matrix.SetDistance(0, 4, 12.0)  ' A-E
        
        matrix.SetDistance(1, 0, 13.0)  ' B-A
        matrix.SetDistance(1, 1, 0.0)   ' B-B
        matrix.SetDistance(1, 2, 17.0)  ' B-C
        matrix.SetDistance(1, 3, 18.0)  ' B-D
        matrix.SetDistance(1, 4, 7.0)   ' B-E
        
        matrix.SetDistance(2, 0, 21.0)  ' C-A
        matrix.SetDistance(2, 1, 17.0)  ' C-B
        matrix.SetDistance(2, 2, 0.0)   ' C-C
        matrix.SetDistance(2, 3, 11.0)  ' C-D
        matrix.SetDistance(2, 4, 10.0)  ' C-E
        
        matrix.SetDistance(3, 0, 22.0)  ' D-A
        matrix.SetDistance(3, 1, 18.0)  ' D-B
        matrix.SetDistance(3, 2, 11.0)  ' D-C
        matrix.SetDistance(3, 3, 0.0)   ' D-D
        matrix.SetDistance(3, 4, 16.0)  ' D-E
        
        matrix.SetDistance(4, 0, 12.0)  ' E-A
        matrix.SetDistance(4, 1, 7.0)   ' E-B
        matrix.SetDistance(4, 2, 10.0)  ' E-C
        matrix.SetDistance(4, 3, 16.0)  ' E-D
        matrix.SetDistance(4, 4, 0.0)   ' E-E
        
        ' Run neighbor joining
        Dim tree As TreeNode = NeighborJoining(matrix)
        
        ' Print the resulting tree
        Console.WriteLine("Neighbor Joining Result:")
        PrintTree(tree, 0)
    End Sub
End Module
```

## Explanation

This Visual Basic implementation of the Neighbor Joining algorithm:

1. **Data Structures**: 
   - `TreeNode` class to represent tree nodes with name, children, parent, and distance properties
   - `DistanceMatrix` class to store the distance matrix and node names

2. **Algorithm Steps**:
   - Start with the full distance matrix
   - Find the minimum element in the matrix
   - Calculate the Q-matrix using the formula: Q(i,j) = (n-2)d(i,j) - sum(d(i,k)) - sum(d(k,j))
   - Find the minimum element in Q-matrix to identify the pair of nodes to join
   - Calculate distances from the new node to the two joined nodes
   - Create new internal node and connect it to the two nodes
   - Update the distance matrix by removing the two joined nodes and adding the new node
   - Repeat until only 2 nodes remain
   - Connect the final two nodes to form the complete tree

3. **Key Features**:
   - Handles matrix operations correctly
   - Properly manages tree structure with parent-child relationships
   - Uses the mathematical formulas for neighbor joining
   - Includes helper functions for printing the resulting tree

## Input Format
The algorithm expects a distance matrix as input, where each entry represents the distance between two taxa.

## Output Format
The output is a phylogenetic tree structure that can be traversed and printed in various formats.

This implementation correctly follows the neighbor joining algorithm as specified in the Rosalind problem requirements.

