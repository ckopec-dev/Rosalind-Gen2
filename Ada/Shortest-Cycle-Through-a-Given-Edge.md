# Solution to Rosalind Problem: Shortest Cycle Through a Given Edge

I'll solve this step by step using Ada programming language.

## Problem Understanding

We need to find the shortest cycle that passes through a given edge in an undirected graph. This is essentially finding the shortest path between two vertices and then adding the edge that connects them to form a cycle.

## Approach

1. Parse input graph data
2. Find shortest path between the two vertices of the given edge using BFS
3. The shortest cycle will be: path from vertex u to v + edge (u,v)
4. Return the length of this cycle

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

procedure Shortest_Cycle_Through_Edge is
   
   type Vertex is new Positive range 1..1000;
   
   type Edge is record
      U, V : Vertex;
   end record;
   
   package Edge_Vectors is new Ada.Containers.Vectors(Vertex, Edge);
   package Vertex_Vectors is new Ada.Containers.Vectors(Vertex, Vertex);
   
   type Graph is record
      Adj_List : array(Vertex) of Vertex_Vectors.Vector;
      Num_Vertices : Positive;
      Num_Edges : Natural;
   end record;
   
   -- BFS to find shortest path between two vertices
   function BFS_Find_Shortest_Path(G : in Graph; Start, Target : Vertex) return Natural is
      type Visited_Array is array(Vertex) of Boolean;
      Visited : Visited_Array := (others => False);
      Queue : array(1..1000) of Vertex;
      Front, Rear : Positive := 1;
      Distance : array(Vertex) of Natural := (others => 0);
      Current_Vertex : Vertex;
   begin
      if Start = Target then
         return 0;
      end if;
      
      Visited(Start) := True;
      Queue(Front) := Start;
      Front := Front + 1;
      
      while Front > Rear loop
         Current_Vertex := Queue(Rear);
         Rear := Rear + 1;
         
         for I in 1..Vertex_Vectors.Length(G.Adj_List(Current_Vertex)) loop
            declare
               Neighbor : Vertex := Vertex_Vectors.Element(G.Adj_List(Current_Vertex), I);
            begin
               if not Visited(Neighbor) then
                  Visited(Neighbor) := True;
                  Distance(Neighbor) := Distance(Current_Vertex) + 1;
                  Queue(Front) := Neighbor;
                  Front := Front + 1;
                  
                  if Neighbor = Target then
                     return Distance(Target);
                  end if;
               end if;
            end;
         end loop;
      end loop;
      
      return Natural'Last; -- No path found
   end BFS_Find_Shortest_Path;
   
   -- Read graph from input
   function Read_Graph return Graph is
      G : Graph;
      N, M : Integer;
      U, V : Vertex;
   begin
      Put_Line("Enter number of vertices and edges:");
      Get(N);
      Get(M);
      
      G.Num_Vertices := N;
      G.Num_Edges := M;
      
      -- Initialize adjacency list
      for I in 1..N loop
         G.Adj_List(I) := Vertex_Vectors.Empty_Vector;
      end loop;
      
      Put_Line("Enter edges:");
      for I in 1..M loop
         Get(U);
         Get(V);
         
         -- Add edge to adjacency lists (undirected graph)
         Vertex_Vectors.Append(G.Adj_List(U), V);
         Vertex_Vectors.Append(G.Adj_List(V), U);
      end loop;
      
      return G;
   end Read_Graph;
   
   -- Main solution
   procedure Solve is
      Graph_Data : Graph;
      Start_Vertex, End_Vertex : Vertex;
      Path_Length : Natural;
      Cycle_Length : Natural;
   begin
      Graph_Data := Read_Graph();
      
      Put_Line("Enter the start and end vertices of the given edge:");
      Get(Start_Vertex);
      Get(End_Vertex);
      
      -- Find shortest path between the two vertices
      Path_Length := BFS_Find_Shortest_Path(Graph_Data, Start_Vertex, End_Vertex);
      
      if Path_Length = Natural'Last then
         Put_Line("No cycle exists through the given edge");
      else
         -- Cycle length = shortest path + 1 (the given edge)
         Cycle_Length := Path_Length + 1;
         Put_Line("Shortest cycle length: ");
         Put(Cycle_Length);
         New_Line;
      end if;
   end Solve;

begin
   Solve;
end Shortest_Cycle_Through_Edge;
```

## Explanation

This Ada solution works as follows:

1. **Data Structures**:
   - `Vertex` type represents graph vertices (1-indexed)
   - `Graph` record stores adjacency list representation of the graph
   - Uses vectors for dynamic arrays

2. **BFS Function**: 
   - Implements breadth-first search to find shortest path between two vertices
   - Returns distance or Natural'Last if no path exists

3. **Main Logic**:
   - Reads graph input (vertices and edges)
   - Takes the given edge vertices as input
   - Finds shortest path between these vertices using BFS
   - The shortest cycle through this edge is: shortest path + 1 (the original edge)

## Time Complexity
- O(V + E) for BFS traversal where V is vertices and E is edges
- Overall complexity is O(V + E)

## Space Complexity
- O(V + E) for storing the graph and BFS data structures

The solution handles the case where there might not be a path between the two vertices of the given edge, returning an appropriate message.