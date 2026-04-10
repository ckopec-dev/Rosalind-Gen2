# Rosalind Problem: Semi-Connected Graph (Visual Basic Solution)

## Problem Understanding

A directed graph is semi-connected if for every pair of vertices u and v, either there is a directed path from u to v or a directed path from v to u (or both).

## Solution Approach

1. **Find Strongly Connected Components (SCCs)** using Kosaraju's algorithm
2. **Create Condensed Graph** by contracting each SCC into a single node
3. **Check if condensed graph is a directed path** - this means it's semi-connected

## Visual Basic Implementation

```vb
Module SemiConnectedGraph
    Sub Main()
        ' Example usage
        Dim adjacencyList As List(Of List(Of Integer)) = New List(Of List(Of Integer)) From {
            New List(Of Integer) From {2, 3},  ' Node 1 -> Nodes 2, 3
            New List(Of Integer) From {4},     ' Node 2 -> Node 4
            New List(Of Integer) From {4},     ' Node 3 -> Node 4
            New List(Of Integer) From {}       ' Node 4 -> None
        }
        
        Dim result As Boolean = IsSemiConnected(adjacencyList)
        Console.WriteLine("Is Semi-Connected: " & result.ToString())
    End Sub
    
    Function IsSemiConnected(adjacencyList As List(Of List(Of Integer))) As Boolean
        Dim n As Integer = adjacencyList.Count
        
        ' Handle edge cases
        If n <= 1 Then Return True
        
        ' Step 1: Find all strongly connected components using Kosaraju's algorithm
        Dim sccs As List(Of List(Of Integer)) = FindStronglyConnectedComponents(adjacencyList)
        
        ' Step 2: Create condensed graph (contract SCCs to single nodes)
        Dim condensedGraph As List(Of List(Of Integer)) = CreateCondensedGraph(adjacencyList, sccs)
        
        ' Step 3: Check if condensed graph forms a directed path
        Return IsDirectedPath(condensedGraph)
    End Function
    
    Function FindStronglyConnectedComponents(adjacencyList As List(Of List(Of Integer))) As List(Of List(Of Integer))
        Dim n As Integer = adjacencyList.Count
        Dim visited As Boolean() = New Boolean(n - 1) {}
        Dim stack As New Stack(Of Integer)()
        Dim finishTime As Integer() = New Integer(n - 1) {}
        Dim time As Integer = 0
        
        ' First DFS to get finishing times
        For i As Integer = 0 To n - 1
            If Not visited(i) Then
                DFSFinishTime(adjacencyList, visited, stack, i, time)
            End If
        Next
        
        ' Create transpose graph
        Dim transpose As List(Of List(Of Integer)) = GetTranspose(adjacencyList)
        
        ' Reset visited array
        For i As Integer = 0 To n - 1
            visited(i) = False
        Next
        
        ' Second DFS on transpose graph in reverse finishing time order
        Dim sccs As New List(Of List(Of Integer))()
        While stack.Count > 0
            Dim node As Integer = stack.Pop()
            If Not visited(node) Then
                Dim scc As New List(Of Integer)()
                DFSSCC(transpose, visited, node, scc)
                sccs.Add(scc)
            End If
        End While
        
        Return sccs
    End Function
    
    Sub DFSFinishTime(adjacencyList As List(Of List(Of Integer)), visited() As Boolean, stack As Stack(Of Integer), node As Integer, ByRef time As Integer)
        visited(node) = True
        time += 1
        
        For Each neighbor As Integer In adjacencyList(node)
            If Not visited(neighbor) Then
                DFSFinishTime(adjacencyList, visited, stack, neighbor, time)
            End If
        Next
        
        time += 1
        stack.Push(node)
    End Sub
    
    Function GetTranspose(adjacencyList As List(Of List(Of Integer))) As List(Of List(Of Integer))
        Dim n As Integer = adjacencyList.Count
        Dim transpose As List(Of List(Of Integer)) = New List(Of List(Of Integer))()
        
        For i As Integer = 0 To n - 1
            transpose.Add(New List(Of Integer)())
        Next
        
        For i As Integer = 0 To n - 1
            For Each neighbor As Integer In adjacencyList(i)
                transpose(neighbor).Add(i)
            Next
        Next
        
        Return transpose
    End Function
    
    Sub DFSSCC(adjacencyList As List(Of List(Of Integer)), visited() As Boolean, node As Integer, scc As List(Of Integer))
        visited(node) = True
        scc.Add(node)
        
        For Each neighbor As Integer In adjacencyList(node)
            If Not visited(neighbor) Then
                DFSSCC(adjacencyList, visited, neighbor, scc)
            End If
        Next
    End Sub
    
    Function CreateCondensedGraph(adjacencyList As List(Of List(Of Integer)), sccs As List(Of List(Of Integer))) As List(Of List(Of Integer))
        Dim n As Integer = adjacencyList.Count
        Dim sccIndex(n - 1) As Integer
        
        ' Map each node to its SCC index
        For i As Integer = 0 To sccs.Count - 1
            For Each node As Integer In sccs(i)
                sccIndex(node) = i
            Next
        Next
        
        ' Create condensed graph
        Dim condensedGraph As List(Of List(Of Integer)) = New List(Of List(Of Integer))()
        
        For i As Integer = 0 To sccs.Count - 1
            condensedGraph.Add(New List(Of Integer)())
        Next
        
        ' Add edges between SCCs
        Dim visitedEdges As New HashSet(Of String)()
        
        For i As Integer = 0 To n - 1
            For Each neighbor As Integer In adjacencyList(i)
                Dim fromSCC As Integer = sccIndex(i)
                Dim toSCC As Integer = sccIndex(neighbor)
                
                ' Avoid duplicate edges
                Dim edgeKey As String = fromSCC.ToString() & "," & toSCC.ToString()
                If fromSCC <> toSCC AndAlso Not visitedEdges.Contains(edgeKey) Then
                    condensedGraph(fromSCC).Add(toSCC)
                    visitedEdges.Add(edgeKey)
                End If
            Next
        Next
        
        Return condensedGraph
    End Function
    
    Function IsDirectedPath(condensedGraph As List(Of List(Of Integer))) As Boolean
        Dim n As Integer = condensedGraph.Count
        
        ' Handle edge cases
        If n <= 1 Then Return True
        If n = 2 Then Return condensedGraph(0).Count > 0 OrElse condensedGraph(1).Count > 0
        
        ' Check if condensed graph is a directed path
        Dim inDegree(n - 1) As Integer
        Dim outDegree(n - 1) As Integer
        
        ' Calculate in-degrees and out-degrees
        For i As Integer = 0 To n - 1
            outDegree(i) = condensedGraph(i).Count
            For Each neighbor As Integer In condensedGraph(i)
                inDegree(neighbor) += 1
            Next
        Next
        
        ' For a directed path:
        ' - Exactly one node with in-degree 0 (start)
        ' - Exactly one node with out-degree 0 (end)
        ' - All other nodes have in-degree = 1 and out-degree = 1
        Dim startNode As Integer = -1
        Dim endNode As Integer = -1
        Dim middleNodes As Integer = 0
        
        For i As Integer = 0 To n - 1
            If inDegree(i) = 0 AndAlso outDegree(i) = 1 Then
                If startNode = -1 Then
                    startNode = i
                Else
                    Return False ' Multiple start nodes
                End If
            ElseIf inDegree(i) = 1 AndAlso outDegree(i) = 0 Then
                If endNode = -1 Then
                    endNode = i
                Else
                    Return False ' Multiple end nodes
                End If
            ElseIf inDegree(i) = 1 AndAlso outDegree(i) = 1 Then
                middleNodes += 1
            ElseIf inDegree(i) = 0 AndAlso outDegree(i) = 0 Then
                Return False ' Isolated node
            Else
                Return False ' Invalid degree combination
            End If
        Next
        
        ' Check if we have exactly one start, one end, and rest are middle nodes
        If startNode = -1 OrElse endNode = -1 OrElse middleNodes <> n - 2 Then
            Return False
        End If
        
        ' Verify path exists by traversing from start
        Dim visited(n - 1) As Boolean
        Dim current As Integer = startNode
        visited(current) = True
        
        While condensedGraph(current).Count > 0
            Dim nextNode As Integer = condensedGraph(current)(0)
            If visited(nextNode) Then
                Return False ' Cycle detected
            End If
            visited(nextNode) = True
            current = nextNode
        End While
        
        ' Check if all nodes were visited
        For i As Integer = 0 To n - 1
            If Not visited(i) Then
                Return False
            End If
        Next
        
        Return True
    End Function
End Module
```

## Key Points

1. **Kosaraju's Algorithm**: Used to find strongly connected components in O(V + E) time
2. **Condensed Graph**: Created by contracting each SCC into a single node
3. **Path Check**: A directed graph is semi-connected if its condensed graph forms a directed path
4. **Time Complexity**: O(V + E) for Kosaraju's algorithm + O(V + E) for path checking = O(V + E)

## Usage

The solution takes an adjacency list representation of a directed graph and returns `True` if the graph is semi-connected, `False` otherwise. The adjacency list should be 0-indexed, where each element represents the list of neighbors for that node.

