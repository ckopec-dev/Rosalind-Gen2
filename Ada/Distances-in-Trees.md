# Rosalind Problem: Distances_in_Trees (Ada Solution)

## Problem Understanding

The problem asks us to compute the distance between all pairs of nodes in a tree, where the distance is the number of edges on the unique path between two nodes.

## Approach

1. Parse input to build adjacency list representation of the tree
2. For each pair of nodes, find the shortest path using BFS/DFS
3. Return all pairwise distances

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Distances_In_Trees is
   
   type Node_Id is new Positive;
   
   package Node_Vectors is new Ada.Containers.Vectors(Node_Id, Node_Id);
   type Adjacency_List is array(Node_Id) of Node_Vectors.Vector;
   
   -- Read tree from input
   procedure Read_Tree(Tree : out Adjacency_List; N : out Natural);
   
   -- Compute distance between two nodes using BFS
   function Distance(Tree : Adjacency_List; Start, Target : Node_Id; N : Natural) return Natural;
   
   -- Print all distances in matrix format
   procedure Print_Distances(Tree : Adjacency_List; N : Natural);
   
   -- Helper procedures
   procedure Read_Line;
   
   -- Global variables
   Tree : Adjacency_List;
   N : Natural;
   
begin
   Read_Tree(Tree, N);
   Print_Distances(Tree, N);
end Distances_In_Trees;

procedure Read_Tree(Tree : out Adjacency_List; N : out Natural) is
   Line : Unbounded_String;
   First_Node, Second_Node : Node_Id;
begin
   -- Read number of nodes
   Get_Line(Line);
   N := To_Integer(Line);
   
   -- Initialize adjacency list
   for I in 1 .. N loop
      Tree(I) := Node_Vectors.Empty_Vector;
   end loop;
   
   -- Read edges and build adjacency list
   while not End_Of_File loop
      begin
         Get_Line(Line);
         if Length(Line) = 0 then
            exit;
         end if;
         
         declare
            Pos : Natural := 1;
            Num1, Num2 : Integer;
         begin
            -- Parse first number
            while Pos <= Length(Line) and then Character'Pos(Element(Line, Pos)) < 48 loop
               Pos := Pos + 1;
            end loop;
            Num1 := 0;
            while Pos <= Length(Line) and then Character'Pos(Element(Line, Pos)) >= 48 loop
               Num1 := Num1 * 10 + (Character'Pos(Element(Line, Pos)) - 48);
               Pos := Pos + 1;
            end loop;
            
            -- Skip spaces
            while Pos <= Length(Line) and then Character'Pos(Element(Line, Pos)) < 48 loop
               Pos := Pos + 1;
            end loop;
            
            -- Parse second number
            Num2 := 0;
            while Pos <= Length(Line) and then Character'Pos(Element(Line, Pos)) >= 48 loop
               Num2 := Num2 * 10 + (Character'Pos(Element(Line, Pos)) - 48);
               Pos := Pos + 1;
            end loop;
            
            First_Node := Node_Id(Num1);
            Second_Node := Node_Id(Num2);
            
            -- Add edges in both directions
            Node_Vectors.Append(Tree(First_Node), Second_Node);
            Node_Vectors.Append(Tree(Second_Node), First_Node);
         end;
      exception
         when others =>
            null;
      end;
   end loop;
end Read_Tree;

function Distance(Tree : Adjacency_List; Start, Target : Node_Id; N : Natural) return Natural is
   type Visited_Array is array(Node_Id range 1 .. N) of Boolean;
   type Queue_Type is array(1 .. N * 2) of Node_Id;
   
   Visited : Visited_Array := (others => False);
   Queue : Queue_Type;
   Head, Tail : Positive := 1;
   Distance : Natural := 0;
   Current_Node : Node_Id;
begin
   -- Initialize queue with start node
   Queue(Head) := Start;
   Visited(Start) := True;
   
   loop
      exit when Head > Tail;
      
      Current_Node := Queue(Head);
      Head := Head + 1;
      
      if Current_Node = Target then
         return Distance;
      end if;
      
      -- Visit all neighbors
      for I in 1 .. Node_Vectors.Length(Tree(Current_Node)) loop
         declare
            Neighbor : Node_Id := Node_Vectors.Element(Tree(Current_Node), I);
         begin
            if not Visited(Neighbor) then
               Visited(Neighbor) := True;
               Queue(Tail + 1) := Neighbor;
               Tail := Tail + 1;
            end if;
         end;
      end loop;
      
      Distance := Distance + 1;
   end loop;
   
   return N; -- Should not happen in a tree
end Distance;

procedure Print_Distances(Tree : Adjacency_List; N : Natural) is
   Distance_Value : Natural;
begin
   for I in 1 .. N loop
      for J in 1 .. N loop
         if J > 1 then
            Put(" ");
         end if;
         Distance_Value := Distance(Tree, Node_Id(I), Node_Id(J), N);
         Put(Distance_Value, Width => 0);
      end loop;
      New_Line;
   end loop;
end Print_Distances;

procedure Read_Line is
   Line : Unbounded_String;
begin
   Get_Line(Line);
end Read_Line;
```

## Explanation

This Ada solution:

1. **Data Structures**: 
   - Uses `Adjacency_List` to represent the tree with an array of vectors
   - Implements a simple BFS algorithm for finding shortest paths

2. **Input Processing**:
   - Reads the number of nodes from first line
   - Parses edges and builds bidirectional adjacency list representation

3. **Distance Calculation**:
   - Uses BFS (Breadth-First Search) to find shortest path between any two nodes
   - BFS ensures we get the minimum number of edges between nodes

4. **Output**:
   - Prints all pairwise distances in matrix format as required by Rosalind

The time complexity is O(N³) for computing all pairs of distances, which should be acceptable for reasonably sized inputs.