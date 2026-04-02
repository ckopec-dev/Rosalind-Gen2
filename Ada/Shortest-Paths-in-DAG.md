# Rosalind Problem: Shortest Paths in DAG - Ada Solution

## Problem Understanding

The problem asks us to find the shortest paths from a source node to all other nodes in a directed acyclic graph (DAG). This can be solved efficiently using topological sorting followed by dynamic programming.

## Solution Approach

1. Parse the input to build the graph representation
2. Perform topological sorting of the nodes
3. Use dynamic programming with topological order to find shortest paths
4. Handle the case where some nodes are unreachable

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

procedure Shortest_Paths_In_DAG is
   
   type Node_Id is new Integer range 1..1000;
   type Weight is new Integer range -1000..1000;
   
   type Edge is record
      target : Node_Id;
      weight : Weight;
   end record;
   
   package Edge_Vectors is new Ada.Containers.Vectors(Node_Id, Edge);
   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists(Node_Id);
   
   type Graph is record
      adjacency_list : array(Node_Id) of Edge_Vectors.Vector;
      num_nodes      : Node_Id;
   end record;
   
   -- Global variables
   G : Graph;
   distances : array(Node_Id) of Weight;
   visited : array(Node_Id) of Boolean;
   topological_order : Node_Lists.List;
   
   -- Function to initialize graph
   procedure Initialize_Graph(N : Node_Id) is
   begin
      G.num_nodes := N;
      for I in 1..N loop
         Edge_Vectors.Clear(G.adjacency_list(I));
      end loop;
   end Initialize_Graph;
   
   -- Add edge to graph
   procedure Add_Edge(Source, Target : Node_Id; W : Weight) is
   begin
      declare
         New_Edge : Edge := (target => Target, weight => W);
      begin
         Edge_Vectors.Append(G.adjacency_list(Source), New_Edge);
      end;
   end Add_Edge;
   
   -- Topological sort using DFS
   procedure Topological_Sort_Util(Node : Node_Id) is
   begin
      visited(Node) := True;
      
      for I in 1..Edge_Vectors.Length(G.adjacency_list(Node)) loop
         declare
            Edge_Item : Edge := Edge_Vectors.Element(G.adjacency_list(Node), I);
         begin
            if not visited(Edge_Item.target) then
               Topological_Sort_Util(Edge_Item.target);
            end if;
         end;
      end loop;
      
      Node_Lists.Prepend(topological_order, Node);
   end Topological_Sort_Util;
   
   -- Perform topological sort
   procedure Topological_Sort is
   begin
      Node_Lists.Clear(topological_order);
      for I in 1..G.num_nodes loop
         visited(I) := False;
      end loop;
      
      for I in 1..G.num_nodes loop
         if not visited(I) then
            Topological_Sort_Util(I);
         end if;
      end loop;
   end Topological_Sort;
   
   -- Find shortest paths using topological sort
   procedure Find_Shortest_Paths(Source : Node_Id) is
      Current_Node : Node_Id;
      Current_Dist : Weight;
      Iterator : Node_Lists.Cursor;
   begin
      -- Initialize distances
      for I in 1..G.num_nodes loop
         distances(I) := 1000000; -- Large number representing infinity
      end loop;
      
      distances(Source) := 0;
      
      -- Process nodes in topological order
      Iterator := Node_Lists.First(topological_order);
      while Node_Lists.Has_Element(Iterator) loop
         Current_Node := Node_Lists.Element(Iterator);
         Current_Dist := distances(Current_Node);
         
         -- Relax all edges from current node
         for I in 1..Edge_Vectors.Length(G.adjacency_list(Current_Node)) loop
            declare
               Edge_Item : Edge := Edge_Vectors.Element(G.adjacency_list(Current_Node), I);
            begin
               if distances(Edge_Item.target) > Current_Dist + Edge_Item.weight then
                  distances(Edge_Item.target) := Current_Dist + Edge_Item.weight;
               end if;
            end;
         end loop;
         
         Iterator := Node_Lists.Next(Iterator);
      end loop;
   end Find_Shortest_Paths;
   
   -- Read input and process
   procedure Process_Input is
      Line : Unbounded_String;
      N, M : Integer;
      Source : Node_Id;
      U, V : Node_Id;
      W : Weight;
   begin
      -- Read number of nodes and edges
      Get_Line(Line);
      declare
         Line_Str : String(1..Line'Length);
      begin
         Line_Str := To_String(Line);
         N := Integer'Value(Line_Str);
      end;
      
      -- Read number of edges
      Get_Line(Line);
      declare
         Line_Str : String(1..Line'Length);
      begin
         Line_Str := To_String(Line);
         M := Integer'Value(Line_Str);
      end;
      
      -- Initialize graph
      Initialize_Graph(Node_Id(N));
      
      -- Read edges
      for I in 1..M loop
         Get_Line(Line);
         declare
            Line_Str : String(1..Line'Length);
            Pos1, Pos2 : Natural;
         begin
            Line_Str := To_String(Line);
            
            -- Find first space
            Pos1 := Line_Str'First;
            while Pos1 <= Line_Str'Last and then Line_Str(Pos1) = ' ' loop
               Pos1 := Pos1 + 1;
            end loop;
            
            Pos2 := Pos1;
            while Pos2 <= Line_Str'Last and then Line_Str(Pos2) /= ' ' loop
               Pos2 := Pos2 + 1;
            end loop;
            
            U := Node_Id(Integer'Value(Line_Str(Pos1..Pos2-1)));
            
            Pos1 := Pos2 + 1;
            while Pos1 <= Line_Str'Last and then Line_Str(Pos1) = ' ' loop
               Pos1 := Pos1 + 1;
            end loop;
            
            Pos2 := Pos1;
            while Pos2 <= Line_Str'Last and then Line_Str(Pos2) /= ' ' loop
               Pos2 := Pos2 + 1;
            end loop;
            
            V := Node_Id(Integer'Value(Line_Str(Pos1..Pos2-1)));
            
            Pos1 := Pos2 + 1;
            while Pos1 <= Line_Str'Last and then Line_Str(Pos1) = ' ' loop
               Pos1 := Pos1 + 1;
            end loop;
            
            W := Weight(Integer'Value(Line_Str(Pos1..Line_Str'Last)));
            
            Add_Edge(U, V, W);
         end;
      end loop;
      
      -- Read source node
      Get_Line(Line);
      declare
         Line_Str : String(1..Line'Length);
      begin
         Line_Str := To_String(Line);
         Source := Node_Id(Integer'Value(Line_Str));
      end;
      
      -- Perform topological sort
      Topological_Sort;
      
      -- Find shortest paths
      Find_Shortest_Paths(Source);
      
      -- Output results
      for I in 1..G.num_nodes loop
         if distances(I) = 1000000 then
            Put("x ");
         else
            Put(distances(I), Width => 0);
            Put(" ");
         end if;
      end loop;
      New_Line;
   end Process_Input;
   
begin
   Process_Input;
end Shortest_Paths_In_DAG;
```

## Key Features of the Solution

1. **Graph Representation**: Uses adjacency lists with vectors for efficient edge storage
2. **Topological Sort**: Implements DFS-based topological sorting to process nodes in correct order
3. **Dynamic Programming**: Uses the topological order to relax edges and find shortest paths
4. **Input Parsing**: Handles the specific input format of Rosalind problems
5. **Distance Handling**: Uses a large number to represent infinity and handles unreachable nodes

## Time Complexity
- Time: O(V + E) where V is number of vertices and E is number of edges
- Space: O(V + E) for graph storage and auxiliary data structures

## Notes
- The solution assumes node IDs start from 1
- Unreachable nodes are marked with "x" as required by Rosalind format
- The algorithm leverages the DAG property to efficiently compute shortest paths without negative cycles

