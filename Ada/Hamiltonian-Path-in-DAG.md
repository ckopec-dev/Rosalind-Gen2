# Rosalind Problem: Hamiltonian Path in DAG - Ada Solution

## Problem Understanding

A Hamiltonian path in a directed graph is a path that visits each vertex exactly once. For a Directed Acyclic Graph (DAG), we need to find if there exists a path that visits all vertices exactly once.

## Approach

1. Use topological sorting to order vertices
2. Apply dynamic programming with bitmasks to track which vertices have been visited
3. Check if we can visit all vertices in the topological order

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure Hamiltonian_Path_In_DAG is
   
   type Vertex is new Integer range 1 .. 1000;
   
   type Edge is record
      From, To : Vertex;
   end record;
   
   package Edge_Vector is new Ada.Containers.Indefinite_Vectors(Vertex, Edge);
   package Adjacency_List is new Ada.Containers.Vectors(Vertex, Vertex);
   
   type Graph is record
      Vertices : Integer;
      Edges    : Edge_Vector.Vector;
      Adj_List : array(Vertex) of Adjacency_List.Vector;
   end record;
   
   procedure Add_Edge(G : in out Graph; From, To : Vertex) is
   begin
      G.Adj_List(From).Append(To);
   end Add_Edge;
   
   procedure Read_Graph(G : in out Graph) is
      Line : String(1 .. 1000);
      Last : Integer;
      V, E : Integer;
   begin
      Get_Line(Line, Last);
      V := 0; E := 0;
      
      -- Parse vertices and edges from input
      for I in 1 .. Last loop
         if Line(I) = ' ' then
            V := V * 10 + (Integer'Value(Line(1 .. I-1)));
            exit;
         end if;
      end loop;
      
      G.Vertices := V;
      
      -- Read edges and build adjacency list
      for I in 1 .. V loop
         declare
            From, To : Vertex;
         begin
            Get_Line(Line, Last);
            From := Vertex'Value(Line(1 .. Line'First + 1));
            To := Vertex'Value(Line(Line'First + 3 .. Last));
            Add_Edge(G, From, To);
         end;
      end loop;
   end Read_Graph;
   
   function Has_Hamiltonian_Path(G : Graph) return Boolean is
      -- Topological sort using Kahn's algorithm
      In_Degree : array(Vertex) of Integer := (others => 0);
      Queue     : array(1 .. G.Vertices) of Vertex;
      Head, Tail : Integer := 0;
      
      -- DP with bitmasks
      type Mask is mod 2**G.Vertices;
      DP : array(Mask) of Boolean := (others => False);
      
      procedure Enqueue(V : Vertex) is
      begin
         Tail := Tail + 1;
         Queue(Tail) := V;
      end Enqueue;
      
      function Dequeue return Vertex is
         V : Vertex;
      begin
         Head := Head + 1;
         V := Queue(Head);
         return V;
      end Dequeue;
      
      procedure Topological_Sort is
         U : Vertex;
      begin
         -- Calculate in-degrees
         for I in 1 .. G.Vertices loop
            In_Degree(I) := 0;
         end loop;
         
         for E of G.Edges loop
            In_Degree(E.To) := In_Degree(E.To) + 1;
         end loop;
         
         -- Initialize queue with vertices having in-degree 0
         Head := 0; Tail := 0;
         for I in 1 .. G.Vertices loop
            if In_Degree(I) = 0 then
               Enqueue(I);
            end if;
         end loop;
      end Topological_Sort;
      
   begin
      -- Special case: single vertex
      if G.Vertices = 1 then
         return True;
      end if;
      
      -- Topological sort
      Topological_Sort;
      
      -- Check if there's a unique topological ordering (which would be Hamiltonian)
      -- This approach uses bitmasks for DP
      declare
         Full_Mask : Mask := (1 => 0, others => 1);
         Current_Mask : Mask;
         U : Vertex;
      begin
         -- Initialize DP: we can start from any vertex with in-degree 0
         for I in 1 .. G.Vertices loop
            if In_Degree(I) = 0 then
               Current_Mask := (others => 0);
               Current_Mask(I) := 1;
               DP(Current_Mask) := True;
            end if;
         end loop;
         
         -- Process vertices in topological order
         while Head < Tail loop
            U := Dequeue;
            
            -- For each vertex that can be reached from U
            for V of G.Adj_List(U) loop
               declare
                  New_Mask : Mask := (others => 0);
               begin
                  -- Copy current mask and set new vertex as visited
                  for I in 1 .. G.Vertices loop
                     if DP(Current_Mask) and (Current_Mask(I) = 1) then
                        New_Mask(I) := 1;
                     end if;
                  end loop;
                  New_Mask(V) := 1;
                  
                  -- If we haven't computed this state yet, compute it
                  if not DP(New_Mask) then
                     DP(New_Mask) := True;
                  end if;
               end;
            end loop;
         end loop;
         
         -- Check if we can reach all vertices (complete mask)
         Full_Mask := (others => 1);
         return DP(Full_Mask);
      end;
   end Has_Hamiltonian_Path;
   
   procedure Solve is
      G : Graph;
   begin
      Read_Graph(G);
      
      if Has_Hamiltonian_Path(G) then
         Put_Line("1");
      else
         Put_Line("0");
      end if;
   end Solve;
   
begin
   Solve;
end Hamiltonian_Path_In_DAG;
```

## Key Insights

1. **Topological Sort**: Since we're dealing with a DAG, we can use topological ordering to process vertices in the correct sequence.

2. **Dynamic Programming with Bitmasks**: We use bitmask DP where each bit represents whether a vertex has been visited in the current path.

3. **Hamiltonian Path Detection**: In a DAG, if there exists a Hamiltonian path, it must follow a topological order, so we can use this constraint to simplify our search.

## Time and Space Complexity

- **Time Complexity**: O(2^N × N) where N is the number of vertices
- **Space Complexity**: O(2^N + E) where E is the number of edges

This approach efficiently determines whether a Hamiltonian path exists in the given DAG.