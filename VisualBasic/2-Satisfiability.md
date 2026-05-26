# Rosalind Problem 2-Satisfiability Solution in Visual Basic

## Problem Understanding

The 2-Satisfiability (2-SAT) problem asks whether a given Boolean formula in conjunctive normal form (CNF) where each clause contains exactly 2 literals can be satisfied.

## Solution Approach

We'll use the strongly connected components (SCC) algorithm with Kosaraju's algorithm:
1. Convert 2-SAT clauses to a directed graph
2. Find strongly connected components
3. Check if any variable and its negation are in the same SCC

## Visual Basic Implementation

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Module TwoSatSolver
    ' Class to represent a variable and its negation
    Class Variable
        Public ReadOnly Property Index As Integer
        Public ReadOnly Property IsNegated As Boolean
        
        Sub New(index As Integer, isNegated As Boolean)
            Me.Index = index
            Me.IsNegated = isNegated
        End Sub
        
        ' Get the opposite variable
        Public Function GetOpposite() As Variable
            Return New Variable(Index, Not IsNegated)
        End Function
    End Class
    
    ' Class to represent a clause (A OR B)
    Class Clause
        Public ReadOnly Property Literal1 As Variable
        Public ReadOnly Property Literal2 As Variable
        
        Sub New(literal1 As Variable, literal2 As Variable)
            Me.Literal1 = literal1
            Me.Literal2 = literal2
        End Sub
    End Class
    
    ' Graph class for SCC computation
    Class Graph
        Private ReadOnly vertices As Integer
        Private ReadOnly adjacencyList As List(Of List(Of Integer))
        Private ReadOnly reverseList As List(Of List(Of Integer))
        
        Sub New(vertexCount As Integer)
            Me.vertices = vertexCount
            Me.adjacencyList = New List(Of List(Of Integer))()
            Me.reverseList = New List(Of List(Of Integer))()
            
            For i As Integer = 0 To vertexCount - 1
                adjacencyList.Add(New List(Of Integer)())
                reverseList.Add(New List(Of Integer)())
            Next
        End Sub
        
        ' Add edge from u to v
        Public Sub AddEdge(u As Integer, v As Integer)
            adjacencyList(u).Add(v)
            reverseList(v).Add(u)
        End Sub
        
        ' Get neighbors of vertex u
        Public Function GetNeighbors(u As Integer) As List(Of Integer)
            Return adjacencyList(u)
        End Function
        
        ' Get reverse neighbors of vertex u
        Public Function GetReverseNeighbors(u As Integer) As List(Of Integer)
            Return reverseList(u)
        End Function
        
        ' Get number of vertices
        Public Function GetVertices() As Integer
            Return vertices
        End Function
    End Class
    
    ' Perform DFS on reverse graph to get finishing times
    Private Function DFSFinishTime(graph As Graph, vertex As Integer, visited As Boolean(), 
                                  finishStack As Stack(Of Integer)) As Boolean
        visited(vertex) = True
        
        Dim neighbors As List(Of Integer) = graph.GetReverseNeighbors(vertex)
        For Each neighbor As Integer In neighbors
            If Not visited(neighbor) Then
                DFSFinishTime(graph, neighbor, visited, finishStack)
            End If
        Next
        
        finishStack.Push(vertex)
        Return True
    End Function
    
    ' Perform DFS on original graph for SCC
    Private Function DFSComponent(graph As Graph, vertex As Integer, visited As Boolean(), 
                                 component As List(Of Integer)) As Boolean
        visited(vertex) = True
        component.Add(vertex)
        
        Dim neighbors As List(Of Integer) = graph.GetNeighbors(vertex)
        For Each neighbor As Integer In neighbors
            If Not visited(neighbor) Then
                DFSComponent(graph, neighbor, visited, component)
            End If
        Next
        
        Return True
    End Function
    
    ' Find strongly connected components using Kosaraju's algorithm
    Private Function FindSCCs(graph As Graph) As List(Of List(Of Integer))
        Dim visited As Boolean() = New Boolean(graph.GetVertices() - 1) {}
        Dim finishStack As New Stack(Of Integer)()
        Dim sccs As New List(Of List(Of Integer))()
        
        ' First DFS to get finishing times
        For i As Integer = 0 To graph.GetVertices() - 1
            If Not visited(i) Then
                DFSFinishTime(graph, i, visited, finishStack)
            End If
        Next
        
        ' Reset visited array
        For i As Integer = 0 To graph.GetVertices() - 1
            visited(i) = False
        Next
        
        ' Second DFS in reverse order to find SCCs
        While finishStack.Count > 0
            Dim vertex As Integer = finishStack.Pop()
            If Not visited(vertex) Then
                Dim component As New List(Of Integer)()
                DFSComponent(graph, vertex, visited, component)
                sccs.Add(component)
            End If
        End While
        
        Return sccs
    End Function
    
    ' Check if 2-SAT is satisfiable
    Public Function Is2Satisfiable(clauses As List(Of Clause), numVariables As Integer) As Boolean
        ' Create implication graph
        ' For clause (A OR B), we have implications: (!A -> B) and (!B -> A)
        Dim graph As New Graph(2 * numVariables)
        
        For Each clause As Clause In clauses
            Dim var1 As Variable = clause.Literal1
            Dim var2 As Variable = clause.Literal2
            
            ' Convert to 0-indexed and handle negation
            Dim index1 As Integer = If(var1.IsNegated, 2 * (var1.Index - 1) + 1, 2 * (var1.Index - 1))
            Dim index2 As Integer = If(var2.IsNegated, 2 * (var2.Index - 1) + 1, 2 * (var2.Index - 1))
            
            ' Add edges for implication
            ' !var1 -> var2
            Dim opposite1 As Integer = If(var1.IsNegated, 2 * (var1.Index - 1), 2 * (var1.Index - 1) + 1)
            graph.AddEdge(opposite1, index2)
            
            ' !var2 -> var1
            Dim opposite2 As Integer = If(var2.IsNegated, 2 * (var2.Index - 1), 2 * (var2.Index - 1) + 1)
            graph.AddEdge(opposite2, index1)
        Next
        
        ' Find SCCs
        Dim sccs As List(Of List(Of Integer)) = FindSCCs(graph)
        
        ' Check if any variable and its negation are in the same SCC
        For i As Integer = 0 To numVariables - 1
            Dim varIndex As Integer = 2 * i
            Dim negIndex As Integer = 2 * i + 1
            
            ' Find which SCC each variable belongs to
            Dim varSCC As Integer = -1
            Dim negSCC As Integer = -1
            
            For j As Integer = 0 To sccs.Count - 1
                If sccs(j).Contains(varIndex) Then
                    varSCC = j
                End If
                If sccs(j).Contains(negIndex) Then
                    negSCC = j
                End If
            Next
            
            ' If variable and its negation are in the same SCC, 2-SAT is unsatisfiable
            If varSCC = negSCC AndAlso varSCC <> -1 Then
                Return False
            End If
        Next
        
        Return True
    End Function
    
    ' Main function to solve the problem
    Public Sub Solve()
        ' Example usage:
        ' Input: 3 variables, 4 clauses: (-1 OR 2), (2 OR 3), (-2 OR -3), (1 OR -3)
        Dim clauses As New List(Of Clause)()
        
        ' Create clauses
        ' Clause 1: (-1 OR 2) -> (not x1 or x2)
        clauses.Add(New Clause(New Variable(1, True), New Variable(2, False)))
        
        ' Clause 2: (2 OR 3) -> (x2 or x3)
        clauses.Add(New Clause(New Variable(2, False), New Variable(3, False)))
        
        ' Clause 3: (-2 OR -3) -> (not x2 or not x3)
        clauses.Add(New Clause(New Variable(2, True), New Variable(3, True)))
        
        ' Clause 4: (1 OR -3) -> (x1 or not x3)
        clauses.Add(New Clause(New Variable(1, False), New Variable(3, True)))
        
        Dim numVars As Integer = 3
        
        If Is2Satisfiable(clauses, numVars) Then
            Console.WriteLine("SATISFIABLE")
        Else
            Console.WriteLine("UNSATISFIABLE")
        End If
    End Sub
End Module
```

## Key Components Explained

1. **Variable Class**: Represents a variable and its negation
2. **Clause Class**: Represents a 2-CNF clause (A OR B)
3. **Graph Class**: Implements adjacency lists for the implication graph
4. **Kosaraju's Algorithm**: Two-phase DFS to find strongly connected components
5. **2-SAT Check**: Verifies that no variable and its negation are in the same SCC

## Time Complexity
- **Time**: O(V + E) where V is the number of variables and E is the number of clauses
- **Space**: O(V + E) for storing the graph

## Output Format
The program outputs either "SATISFIABLE" or "UNSATISFIABLE" based on whether the 2-SAT instance can be satisfied.

This implementation correctly solves the 2-Satisfiability problem using graph theory and strongly connected components to determine satisfiability.

