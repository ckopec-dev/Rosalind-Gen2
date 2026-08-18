# Rosalind Problem: Implement Tree Coloring (Ada Solution)

## Problem Understanding

The Tree Coloring problem involves coloring nodes of a tree such that no two adjacent nodes have the same color, using the minimum number of colors (chromatic number). This is essentially finding the graph coloring of a tree.

## Solution Approach

For trees specifically, we can use a greedy coloring algorithm:
1. Process nodes in a specific order (like BFS/DFS)
2. For each node, assign the smallest available color that doesn't conflict with neighbors
3. Since trees are bipartite, we only need 2 colors maximum

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure TreeColoring is
   
   -- Define node type and adjacency list structure
   type Node_Id is new Integer range 0..1000;
   type Color is new Integer range 1..1000;
   
   -- Vector to store neighbors for each node
   package Neighbors_Vector is new Ada.Containers.Vectors(Node_Id, Node_Id);
   type Neighbors_Array is array(Node_Id) of Neighbors_Vector.Vector;
   
   -- Map to store node colors
   package Color_Map is new Ada.Containers.Ordered_Maps(Node_Id, Color);
   type Color_Array is array(Node_Id) of Color;
   
   -- Global variables
   Nodes_Count : Integer := 0;
   Adjacency_List : Neighbors_Array;
   Colors : Color_Array;
   Visited : array(Node_Id) of Boolean := (others => False);
   
   -- Function to add edge to adjacency list
   procedure Add_Edge(U, V : Node_Id) is
   begin
      Neighbors_Vector.Append(Adjacency_List(U), V);
      Neighbors_Vector.Append(Adjacency_List(V), U);
   end Add_Edge;
   
   -- Function to get available colors for a node
   function Get_Available_Color(Node : Node_Id) return Color is
      Used_Colors : array(1..Nodes_Count) of Boolean := (others => False);
      Neighbors : Neighbors_Vector.Vector renames Adjacency_List(Node);
      Neighbor : Node_Id;
      Max_Color : Color := 1;
   begin
      -- Mark colors used by neighbors
      for I in 1..Neighbors_Vector.Length(Neighbors) loop
         Neighbor := Neighbors_Vector.Element(Neighbors, I);
         if Colors(Neighbor) /= 0 then
            Used_Colors(Colors(Neighbor)) := True;
         end if;
      end loop;
      
      -- Find first available color
      for C in 1..Nodes_Count loop
         if not Used_Colors(C) then
            return Color(C);
         end if;
      end loop;
      
      return 1; -- Should never reach here for valid tree
   end Get_Available_Color;
   
   -- Greedy coloring function
   procedure Color_Tree is
      Root : Node_Id := 0;
   begin
      -- Initialize all colors to 0 (unassigned)
      for I in 0..Nodes_Count loop
         Colors(I) := 0;
      end loop;
      
      -- Simple greedy coloring - assign colors to nodes one by one
      for Node in 0..Nodes_Count-1 loop
         Colors(Node) := Get_Available_Color(Node);
      end loop;
   end Color_Tree;
   
   -- Read input from stdin
   procedure Read_Input is
      Line : String(1..1000);
      Last : Natural;
      U, V : Node_Id;
      N : Integer;
   begin
      -- Read number of nodes
      Get_Line(Line, Last);
      N := Integer'Value(Line(1..Last));
      Nodes_Count := N - 1; -- Adjust for 0-based indexing
      
      -- Initialize adjacency list
      for I in 0..Nodes_Count loop
         Neighbors_Vector.Clear(Adjacency_List(I));
      end loop;
      
      -- Read edges
      while not End_Of_File loop
         begin
            Get_Line(Line, Last);
            if Last > 0 then
               -- Parse two nodes from the line
               declare
                  Space_Pos : Natural := Line'First;
                  First_Num : Integer := 0;
                  Second_Num : Integer := 0;
               begin
                  -- Find space position
                  while Space_Pos <= Last and then Line(Space_Pos) /= ' ' loop
                     Space_Pos := Space_Pos + 1;
                  end loop;
                  
                  if Space_Pos <= Last then
                     First_Num := Integer'Value(Line(1..Space_Pos-1));
                     Second_Num := Integer'Value(Line(Space_Pos+1..Last));
                     U := Node_Id(First_Num);
                     V := Node_Id(Second_Num);
                     Add_Edge(U, V);
                  end if;
               end;
            else
               exit;
            end if;
         exception
            when others => exit;
         end;
      end loop;
   end Read_Input;
   
   -- Output results
   procedure Output_Results is
   begin
      for I in 0..Nodes_Count loop
         Put(Colors(I)); Put(" ");
      end loop;
      New_Line;
   end Output_Results;

begin
   Read_Input;
   Color_Tree;
   Output_Results;
end TreeColoring;
```

## Key Features of the Implementation

1. **Data Structures**: Uses vectors for adjacency lists and arrays for storing node colors
2. **Greedy Algorithm**: Assigns minimum available color to each node based on neighbors
3. **Input/Output Handling**: Reads edges from standard input and outputs coloring results
4. **Error Handling**: Includes basic exception handling for malformed input

## Sample Input/Output

**Input:**
```
4
0 1
1 2
1 3
```

**Output:**
```
1 2 1 1
```

## Time Complexity
- **Time**: O(V × D) where V is the number of vertices and D is the maximum degree
- **Space**: O(V + E) for adjacency list storage

This solution efficiently handles tree coloring using a greedy approach suitable for Rosalind's requirements.