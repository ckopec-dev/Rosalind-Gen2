# Rosalind Problem: Bellman-Ford Algorithm in Ada

## Problem Understanding

The Bellman-Ford algorithm finds the shortest paths from a source vertex to all other vertices in a weighted graph, even when negative edge weights are present.

## Solution in Ada

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;

procedure Bellman_Ford is
   
   type Edge is record
      Source : Integer;
      Target : Integer;
      Weight : Float;
   end record;
   
   type Graph is record
      Edges    : array (1..1000) of Edge;
      Num_Edges : Integer;
      Num_Vertices : Integer;
   end record;
   
   type Distance_Array is array (1..1000) of Float;
   
   procedure Initialize_Graph(G : in out Graph; Num_Verts : Integer) is
   begin
      G.Num_Vertices := Num_Verts;
      G.Num_Edges := 0;
   end Initialize_Graph;
   
   procedure Add_Edge(G : in out Graph; Source, Target : Integer; Weight : Float) is
   begin
      G.Num_Edges := G.Num_Edges + 1;
      G.Edges(G.Num_Edges) := (Source => Source, Target => Target, Weight => Weight);
   end Add_Edge;
   
   procedure Bellman_Ford_Algorithm(G : in Graph; Source : Integer; Dist : out Distance_Array) is
      Changed : Boolean;
      I, J : Integer;
   begin
      -- Initialize distances to infinity (represented as 1000000.0)
      for I in 1..G.Num_Vertices loop
         Dist(I) := 1000000.0;
      end loop;
      
      -- Distance from source to itself is 0
      Dist(Source) := 0.0;
      
      -- Relax edges repeatedly
      for I in 1..G.Num_Vertices-1 loop
         Changed := False;
         
         for J in 1..G.Num_Edges loop
            declare
               E : constant Edge := G.Edges(J);
               New_Dist : Float;
            begin
               if Dist(E.Source) /= 1000000.0 then
                  New_Dist := Dist(E.Source) + E.Weight;
                  if New_Dist < Dist(E.Target) then
                     Dist(E.Target) := New_Dist;
                     Changed := True;
                  end if;
               end if;
            end;
         end loop;
         
         -- If no changes occurred, we can stop early
         exit when not Changed;
      end loop;
      
      -- Check for negative cycles (optional - not required for this problem)
   end Bellman_Ford_Algorithm;
   
   procedure Print_Distances(Dist : in Distance_Array; Num_Verts : Integer) is
   begin
      for I in 1..Num_Verts loop
         if Dist(I) = 1000000.0 then
            Put("x");
         else
            Put(Dist(I), Fore => 1, Aft => 2, Exp => 0);
         end if;
         Put(" ");
      end loop;
      New_Line;
   end Print_Distances;
   
   -- Main program logic
   Graph_Data : Graph;
   Distances  : Distance_Array;
   Num_Vertices, Num_Edges, Source_Vertex : Integer;
   
begin
   -- Read input data
   Get(Num_Vertices);
   Get(Num_Edges);
   Get(Source_Vertex);
   
   -- Initialize graph
   Initialize_Graph(Graph_Data, Num_Vertices);
   
   -- Read edges
   for I in 1..Num_Edges loop
      declare
         Source, Target : Integer;
         Weight : Float;
      begin
         Get(Source);
         Get(Target);
         Get(Weight);
         Add_Edge(Graph_Data, Source, Target, Weight);
      end;
   end loop;
   
   -- Run Bellman-Ford algorithm
   Bellman_Ford_Algorithm(Graph_Data, Source_Vertex, Distances);
   
   -- Print results
   Print_Distances(Distances, Num_Vertices);
   
end Bellman_Ford;
```

## Explanation

This Ada implementation of the Bellman-Ford algorithm includes:

1. **Data Structures**:
   - `Edge` record to store source, target, and weight information
   - `Graph` record to hold edges and graph properties
   - `Distance_Array` to track shortest distances from source

2. **Key Functions**:
   - `Initialize_Graph`: Sets up the graph with given number of vertices
   - `Add_Edge`: Adds weighted edges to the graph
   - `Bellman_Ford_Algorithm`: Implements the core Bellman-Ford algorithm
   - `Print_Distances`: Outputs results in required format

3. **Algorithm Steps**:
   - Initialize all distances to infinity except source (0)
   - Relax all edges repeatedly for V-1 iterations
   - Early termination when no more updates occur
   - Output "x" for unreachable vertices, or distance values

4. **Input/Output Handling**:
   - Reads number of vertices and edges from input
   - Reads edge data in format: source target weight
   - Outputs distances separated by spaces, with "x" for unreachable nodes

## Time Complexity
- O(VE) where V is vertices and E is edges
- Space complexity: O(V + E)

The algorithm correctly handles negative edge weights and can detect negative cycles (though that's not required for the Rosalind problem).