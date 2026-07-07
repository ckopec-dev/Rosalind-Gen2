# Rosalind Problem: Degree Array (Ada Solution)

## Problem Understanding

The degree of a vertex in an undirected graph is the number of edges incident to that vertex. Given an undirected graph with n vertices and m edges, we need to compute the degree of each vertex.

## Solution Approach

1. Read the graph structure from input
2. Count the degree of each vertex by counting incident edges
3. Output the degrees in order (1 to n)

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Degree_Array is
   type Vertex is new Integer range 1 .. 10000;
   
   package Vertex_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Vertex);
   
   -- Read number of vertices and edges
   N : Integer;
   M : Integer;
   
   -- Array to store degrees of each vertex
   Degree : array (1 .. 10000) of Natural := (others => 0);
   
begin
   -- Read n and m
   Get(N);
   Get(M);
   
   -- Read edges and update degrees
   for i in 1 .. M loop
      declare
         U, V : Vertex;
      begin
         Get(U);
         Get(V);
         Degree(U) := Degree(U) + 1;
         Degree(V) := Degree(V) + 1;
      end;
   end loop;
   
   -- Output degrees of all vertices
   for i in 1 .. N loop
      Put(Degree(i));
      Put(" ");
   end loop;
   
   New_Line;
end Degree_Array;
```

## Input/Output Format

**Input:**
- First line: two integers n and m (number of vertices and edges)
- Next m lines: pairs of integers u v representing an edge between vertices u and v

**Output:**
- Single line with n integers representing the degree of each vertex (1 to n)

## Example

**Input:**
```
6 7
1 2
2 3
3 4
4 5
5 6
6 1
2 4
```

**Output:**
```
2 3 2 3 2 2
```

## Explanation

- Vertex 1 is connected to vertices 2 and 6 (degree = 2)
- Vertex 2 is connected to vertices 1, 3, and 4 (degree = 3)
- Vertex 3 is connected to vertices 2 and 4 (degree = 2)
- Vertex 4 is connected to vertices 3, 5, and 2 (degree = 3)
- Vertex 5 is connected to vertices 4 and 6 (degree = 2)
- Vertex 6 is connected to vertices 5 and 1 (degree = 2)

The solution has O(n + m) time complexity where n is the number of vertices and m is the number of edges.