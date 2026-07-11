# Rosalind Problem: Dijkstra's Algorithm - Ada Solution

## Problem Understanding

Dijkstra's algorithm finds the shortest paths from a source vertex to all other vertices in a weighted graph. Given a weighted directed graph and a source vertex, we need to output the shortest distances to all vertices.

## Solution Approach

I'll implement Dijkstra's algorithm using:
- An adjacency list representation
- A priority queue (min-heap) for efficient vertex selection
- Distance tracking array
- Previous vertex tracking for path reconstruction

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Priority_Queues;

procedure Dijkstra_Algorithm is
   
   -- Graph representation using adjacency list
   type Edge is record
      Target : Integer;
      Weight : Integer;
   end record;
   
   package Edge_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, 
                                                       Element_Type => Edge);
   
   type Vertex_List is array (Positive range <>) of Edge_Vectors.Vector;
   
   -- Priority queue for vertices
   package Vertex_Queue is new Ada.Priority_Queues (Element_Type => Integer, 
                                                    Priority_Type => Integer,
                                                    "<" => "<");
   
   -- Input parsing
   procedure Read_Graph (Vertices : in out Vertex_List; 
                        Num_Vertices : in out Integer;
                        Num_Edges : in out Integer) is
      Line : String(1..200);
      Last : Natural;
      V1, V2, W : Integer;
   begin
      Get_Line(Line, Last);
      Num_Vertices := Integer'Value(Line(1..Last));
      
      Get_Line(Line, Last);
      Num_Edges := Integer'Value(Line(1..Last));
      
      -- Initialize adjacency lists
      for I in 1..Num_Vertices loop
         Vertices(I) := Edge_Vectors.Empty_Vector;
      end loop;
      
      -- Read edges
      for I in 1..Num_Edges loop
         Get_Line(Line, Last);
         V1 := Integer'Value(Line(1..Last));
         declare
            Pos : Natural := Line'First;
            Temp : String(1..200);
            Temp_Last : Natural;
         begin
            while Pos <= Last and then Line(Pos) = ' ' loop
               Pos := Pos + 1;
            end loop;
            
            -- Parse first vertex
            Temp_Last := 0;
            while Pos <= Last and then Line(Pos) /= ' ' loop
               Temp(Temp_Last+1) := Line(Pos);
               Pos := Pos + 1;
               Temp_Last := Temp_Last + 1;
            end loop;
            V1 := Integer'Value(Temp(1..Temp_Last));
            
            -- Skip spaces
            while Pos <= Last and then Line(Pos) = ' ' loop
               Pos := Pos + 1;
            end loop;
            
            -- Parse second vertex
            Temp_Last := 0;
            while Pos <= Last and then Line(Pos) /= ' ' loop
               Temp(Temp_Last+1) := Line(Pos);
               Pos := Pos + 1;
               Temp_Last := Temp_Last + 1;
            end loop;
            V2 := Integer'Value(Temp(1..Temp_Last));
            
            -- Skip spaces
            while Pos <= Last and then Line(Pos) = ' ' loop
               Pos := Pos + 1;
            end loop;
            
            -- Parse weight
            Temp_Last := 0;
            while Pos <= Last and then Line(Pos) /= ' ' loop
               Temp(Temp_Last+1) := Line(Pos);
               Pos := Pos + 1;
               Temp_Last := Temp_Last + 1;
            end loop;
            W := Integer'Value(Temp(1..Temp_Last));
            
            -- Add edge to adjacency list (0-indexed)
            Vertices(V1).Append((Target => V2, Weight => W));
         end;
      end loop;
   end Read_Graph;
   
   -- Dijkstra's Algorithm
   procedure Dijkstra (Vertices : in out Vertex_List;
                      Source : in Integer;
                      Distances : out array (1..1000) of Integer;
                      Num_Vertices : in Integer) is
      Visited : array (1..1000) of Boolean := (others => False);
      Queue : Vertex_Queue.Queue;
      
      -- Initialize distances
      procedure Initialize_Distances is
      begin
         for I in 1..Num_Vertices loop
            Distances(I) := Integer'Last;  -- Infinity
         end loop;
         Distances(Source) := 0;
      end Initialize_Distances;
      
   begin
      Initialize_Distances;
      
      -- Add source to queue
      Vertex_Queue.Enqueue(Queue, Source, 0);
      
      while not Vertex_Queue.Is_Empty(Queue) loop
         declare
            Current_Vertex : Integer := Vertex_Queue.Dequeue(Queue);
         begin
            if Visited(Current_Vertex) then
               continue;
            end if;
            
            Visited(Current_Vertex) := True;
            
            -- Check all neighbors
            for I in 1..Edge_Vectors.Length(Vertices(Current_Vertex)) loop
               declare
                  Edge : Edge := Edge_Vectors.Element(Vertices(Current_Vertex), I-1);
                  New_Distance : Integer := Distances(Current_Vertex) + Edge.Weight;
               begin
                  if not Visited(Edge.Target) and then 
                     New_Distance < Distances(Edge.Target) then
                     Distances(Edge.Target) := New_Distance;
                     Vertex_Queue.Enqueue(Queue, Edge.Target, New_Distance);
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Dijkstra;
   
   -- Main execution
   procedure Process_Input is
      Num_Vertices : Integer;
      Num_Edges : Integer;
      Vertices : Vertex_List(1..1000);
      Distances : array (1..1000) of Integer;
      Source : Integer;
      Line : String(1..200);
      Last : Natural;
   begin
      -- Read source vertex
      Get_Line(Line, Last);
      Source := Integer'Value(Line(1..Last));
      
      -- Read graph
      Read_Graph(Vertices, Num_Vertices, Num_Edges);
      
      -- Run Dijkstra
      Dijkstra(Vertices, Source, Distances, Num_Vertices);
      
      -- Output results
      for I in 1..Num_Vertices loop
         if Distances(I) = Integer'Last then
            Put("1000000 ");
         else
            Put(Distances(I));
            Put(" ");
         end if;
      end loop;
      New_Line;
   end Process_Input;
   
begin
   Process_Input;
end Dijkstra_Algorithm;
```

## Key Implementation Details

1. **Data Structures**:
   - `Edge` record to store target vertex and edge weight
   - `Edge_Vectors.Vector` for adjacency list representation
   - Priority queue for efficient vertex selection

2. **Algorithm Steps**:
   - Initialize distances with infinity except source (0)
   - Add source to priority queue
   - While queue not empty:
     - Extract minimum distance vertex
     - For each neighbor, update distance if shorter path found
     - Add updated vertices to queue

3. **Output Format**:
   - Space-separated shortest distances to all vertices
   - Use 1000000 for unreachable vertices

## Time Complexity
- **Time**: O((V + E) log V) where V is vertices, E is edges
- **Space**: O(V + E) for adjacency list and auxiliary arrays

This implementation efficiently solves the Dijkstra's algorithm problem on Rosalind using Ada's built-in containers and priority queue facilities.