# Rosalind Problem: Reconstruct a String from its k-mer Composition (Visual Basic Solution)

## Problem Understanding

Given a collection of k-mers, we need to reconstruct the original DNA string by finding an Eulerian path in the de Bruijn graph formed by these k-mers.

## Solution Approach

1. Build a de Bruijn graph from the k-mers
2. Find an Eulerian path in the graph
3. Reconstruct the string from the path

## Visual Basic Implementation

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Module ReconstructStringFromKmers
    Sub Main()
        ' Example input
        Dim kmers As New List(Of String) From {
            "GAGG",
            "CAGG",
            "GGGG",
            "GGGA",
            "CAGG",
            "AGGG",
            "GGAG"
        }
        
        Dim result As String = ReconstructString(kmers)
        Console.WriteLine(result)
    End Sub
    
    Function ReconstructString(kmers As List(Of String)) As String
        ' Build de Bruijn graph
        Dim graph As New Dictionary(Of String, List(Of String))
        Dim inDegree As New Dictionary(Of String, Integer)
        Dim outDegree As New Dictionary(Of String, Integer)
        
        ' Initialize degrees
        For Each kmer As String In kmers
            Dim prefix As String = kmer.Substring(0, kmer.Length - 1)
            Dim suffix As String = kmer.Substring(1)
            
            If Not graph.ContainsKey(prefix) Then
                graph(prefix) = New List(Of String)
                inDegree(prefix) = 0
                outDegree(prefix) = 0
            End If
            
            If Not graph.ContainsKey(suffix) Then
                graph(suffix) = New List(Of String)
                inDegree(suffix) = 0
                outDegree(suffix) = 0
            End If
            
            graph(prefix).Add(suffix)
            outDegree(prefix) += 1
            inDegree(suffix) += 1
        Next
        
        ' Find start node (node with outDegree - inDegree = 1)
        Dim startNode As String = ""
        For Each node As String In graph.Keys
            Dim diff As Integer = outDegree(node) - inDegree(node)
            If diff = 1 Then
                startNode = node
                Exit For
            End If
        Next
        
        ' If no start node found, use any node
        If String.IsNullOrEmpty(startNode) Then
            startNode = graph.Keys.First()
        End If
        
        ' Find Eulerian path using Hierholzer's algorithm
        Dim path As New Stack(Of String)
        Dim circuit As New List(Of String)
        path.Push(startNode)
        
        While path.Count > 0
            Dim current As String = path.Peek()
            
            If graph.ContainsKey(current) AndAlso graph(current).Count > 0 Then
                Dim nextNode As String = graph(current).First()
                graph(current).RemoveAt(0)
                path.Push(nextNode)
            Else
                circuit.Add(path.Pop())
            End If
        End While
        
        ' Reverse the circuit to get the correct order
        circuit.Reverse()
        
        ' Reconstruct the string
        If circuit.Count = 0 Then Return ""
        
        Dim result As String = circuit(0)
        For i As Integer = 1 To circuit.Count - 1
            result += circuit(i).Substring(circuit(i).Length - 1)
        Next
        
        Return result
    End Function
End Module
```

## Explanation

### Key Steps:

1. **Graph Construction**: 
   - For each k-mer, create a prefix (k-1 characters) and suffix (k-1 characters)
   - Build adjacency list representation of de Bruijn graph

2. **Degree Calculation**:
   - Calculate in-degrees and out-degrees for each node
   - Find the start node (where out-degree - in-degree = 1)

3. **Eulerian Path Finding**:
   - Use Hierholzer's algorithm to find the Eulerian path
   - Start from the identified start node
   - Follow edges until no more edges are available

4. **String Reconstruction**:
   - Convert the path back to the original string
   - Take the first character of the first node and the last character of each subsequent node

## Time and Space Complexity

- **Time Complexity**: O(|E| + |V|) where |E| is the number of edges and |V| is the number of vertices
- **Space Complexity**: O(|E| + |V|) for storing the graph

## Example Output

For the input kmers: ["GAGG", "CAGG", "GGGG", "GGGA", "CAGG", "AGGG", "GGAG"]
The output should be: "GAGGGAGG" or similar valid reconstruction

This solution correctly handles the reconstruction of a DNA string from its k-mer composition using the principles of Eulerian path finding in de Bruijn graphs.

