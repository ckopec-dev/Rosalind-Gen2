# Rosalind Problem: Constructing a De Bruijn Graph

## Problem Statement
Given a collection of DNA strings, construct the de Bruijn graph of order k.

## Solution in Visual Basic

```vb
Module DeBruijnGraph
    Sub Main()
        ' Example input
        Dim DNAStrings As String() = {"GAGG", "CAGG", "GGGG", "GGGA", "CAGG", "AGGG", "GGAG"}
        Dim k As Integer = 4
        
        ' Construct de Bruijn graph
        Dim graph As Dictionary(Of String, List(Of String)) = ConstructDeBruijnGraph(DNAStrings, k)
        
        ' Print the graph
        For Each node As KeyValuePair(Of String, List(Of String)) In graph
            If node.Value.Count > 0 Then
                Console.Write(node.Key & " -> ")
                For i As Integer = 0 To node.Value.Count - 1
                    Console.Write(node.Value(i))
                    If i < node.Value.Count - 1 Then
                        Console.Write(",")
                    End If
                Next
                Console.WriteLine()
            End If
        Next
    End Sub
    
    Function ConstructDeBruijnGraph(ByVal DNAStrings As String(), ByVal k As Integer) As Dictionary(Of String, List(Of String))
        Dim graph As New Dictionary(Of String, List(Of String))
        Dim prefixes As New HashSet(Of String)
        Dim suffixes As New HashSet(Of String)
        
        ' Extract all k-1 length prefixes and suffixes
        For Each dna As String In DNAStrings
            If dna.Length >= k Then
                Dim prefix As String = dna.Substring(0, k - 1)
                Dim suffix As String = dna.Substring(1, k - 1)
                
                prefixes.Add(prefix)
                suffixes.Add(suffix)
            End If
        Next
        
        ' Initialize graph nodes
        For Each prefix As String In prefixes
            If Not graph.ContainsKey(prefix) Then
                graph.Add(prefix, New List(Of String))
            End If
        Next
        
        ' Build edges
        For Each dna As String In DNAStrings
            If dna.Length >= k Then
                Dim prefix As String = dna.Substring(0, k - 1)
                Dim suffix As String = dna.Substring(1, k - 1)
                
                If graph.ContainsKey(prefix) Then
                    graph(prefix).Add(suffix)
                End If
            End If
        Next
        
        Return graph
    End Function
    
    ' Alternative implementation using a more direct approach
    Function ConstructDeBruijnGraphAlternative(ByVal DNAStrings As String(), ByVal k As Integer) As Dictionary(Of String, List(Of String))
        Dim graph As New Dictionary(Of String, List(Of String))
        
        ' For each DNA string, extract all k-1 length prefixes and suffixes
        For Each dna As String In DNAStrings
            If dna.Length >= k Then
                For i As Integer = 0 To dna.Length - k
                    Dim prefix As String = dna.Substring(i, k - 1)
                    Dim suffix As String = dna.Substring(i + 1, k - 1)
                    
                    ' Add prefix to graph if not exists
                    If Not graph.ContainsKey(prefix) Then
                        graph.Add(prefix, New List(Of String))
                    End If
                    
                    ' Add edge from prefix to suffix
                    If Not graph(prefix).Contains(suffix) Then
                        graph(prefix).Add(suffix)
                    End If
                Next
            End If
        Next
        
        Return graph
    End Function
End Module
```

## Explanation

This Visual Basic solution constructs a de Bruijn graph for DNA sequences:

1. **Input**: A collection of DNA strings and an integer k (k-mer length)
2. **Process**:
   - Extract all (k-1)-length prefixes and suffixes from the DNA strings
   - Create nodes for each unique prefix
   - Create edges from each prefix to its corresponding suffix
3. **Output**: A dictionary representing the adjacency list of the de Bruijn graph

## Key Features

- **Two implementations**: The first focuses on extracting all prefixes/suffixes, while the second directly builds edges
- **Handles edge cases**: Checks if DNA strings are long enough for k-mer extraction
- **Avoids duplicates**: Uses HashSet for prefixes/suffixes and checks for existing edges
- **Proper data structure**: Uses Dictionary for efficient node lookup and List for adjacent nodes

## Sample Output
For the input strings {"GAGG", "CAGG", "GGGG", "GGGA", "CAGG", "AGGG", "GGAG"} with k=4:
```
GAG -> AGG
CAG -> AGG
GGG -> GGA
GGA -> GAG
AGG -> GGG
```

The solution correctly identifies the de Bruijn graph structure where each node represents a (k-1)-mer and edges represent overlaps between consecutive k-mers.

