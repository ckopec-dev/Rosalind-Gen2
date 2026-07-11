# Rosalind Problem: Testing Acyclicity - Ada Solution

## Problem Understanding

The problem asks us to determine if a directed graph is acyclic (i.e., contains no cycles). This is equivalent to checking if the graph is a Directed Acyclic Graph (DAG).

## Approach

We'll use **topological sorting** with **Kahn's algorithm**:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. Process vertices from the queue, reducing in-degrees of neighbors
4. If we process all vertices, the graph is acyclic; otherwise, it contains cycles

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Testing_Acyclicity is
   
   -- Type definitions for adjacency list representation
   type Vertex is new Integer range 1 .. 1000;
   type Edge is record
      From, To : Vertex;
   end record;
   
   package Edge_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Edge);
   package Vertex_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Vertex);
   
   type Adjacency_List is array (Vertex) of Vertex_Vectors.Vector;
   
   -- Global variables
   N : Integer;  -- Number of vertices
   M : Integer;  -- Number of edges
   In_Degree : array (Vertex) of Integer := (others => 0);
   Graph : Adjacency_List;
   
   -- Function to add an edge to the graph
   procedure Add_Edge (From, To : Vertex) is
   begin
      Graph(From).Append (To);
      In_Degree(To) := In_Degree(To) + 1;
   end Add_Edge;
   
   -- Main algorithm using Kahn's algorithm for topological sort
   function Is_Acyclic return Boolean is
      Queue : Vertex_Vectors.Vector;
      Processed_Count : Integer := 0;
      Current_Vertex : Vertex;
   begin
      -- Initialize queue with vertices having in-degree 0
      for V in 1 .. N loop
         if In_Degree(V) = 0 then
            Queue.Append (V);
         end if;
      end loop;
      
      -- Process vertices in topological order
      while not Queue.Is_Empty loop
         Current_Vertex := Queue.First_Element;
         Queue.Delete_First;
         Processed_Count := Processed_Count + 1;
         
         -- Reduce in-degrees of neighbors
         for I in 1 .. Graph(Current_Vertex).Length loop
            declare
               Neighbor : constant Vertex := Graph(Current_Vertex).Element (I);
            begin
               In_Degree(Neighbor) := In_Degree(Neighbor) - 1;
               if In_Degree(Neighbor) = 0 then
                  Queue.Append (Neighbor);
               end if;
            end;
         end loop;
      end loop;
      
      -- If we processed all vertices, graph is acyclic (no cycles)
      return Processed_Count /= N;
   end Is_Acyclic;
   
begin
   -- Read input
   Get (N); Get (M);
   New_Line;
   
   -- Initialize adjacency list and in-degrees
   for I in 1 .. N loop
      Graph(I) := (others => <>);
      In_Degree(I) := 0;
   end loop;
   
   -- Read edges
   for I in 1 .. M loop
      declare
         From, To : Vertex;
      begin
         Get (From); Get (To);
         Add_Edge (From, To);
      end;
   end loop;
   
   -- Check if graph is acyclic
   if Is_Acyclic then
      Put (1);  -- Contains cycles
   else
      Put (0);  -- No cycles
   end if;
   
   New_Line;
end Testing_Acyclicity;
```

## Explanation

1. **Input Reading**: Read the number of vertices `N` and edges `M`, followed by all edges.

2. **Graph Representation**: 
   - Use an adjacency list representation with vectors
   - Track in-degrees for each vertex

3. **Algorithm**:
   - Initialize queue with all vertices having in-degree 0
   - Process vertices from the queue, reducing in-degrees of neighbors
   - If we can process all vertices, the graph is acyclic (return 0)
   - Otherwise, it contains cycles (return 1)

4. **Time Complexity**: O(V + E) where V is vertices and E is edges
5. **Space Complexity**: O(V + E) for adjacency list and in-degree array

## Sample Input/Output

**Input:**
```
4 4
1 2
2 3
3 1
4 1
```

**Output:**
```
1
```

This indicates the graph contains cycles (it's not acyclic).

**Input:**
```
4 3
1 2
2 3
3 4
```

**Output:**
```
0
```

This indicates the graph is acyclic (DAG).