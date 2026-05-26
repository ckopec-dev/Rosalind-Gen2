# Rosalind Problem: Compute Distances Between Leaves

## Problem Description
Given a weighted tree with n leaves, compute the distance between each pair of leaves.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Compute_Distances_Between_Leaves is
   
   type Node_Id is new Natural;
   type Weight is new Float;
   
   type Edge is record
      to_node : Node_Id;
      weight  : Weight;
   end record;
   
   package Edge_Vectors is new Ada.Containers.Vectors(Node_Id, Edge);
   type Adjacency_List is array(Node_Id) of Edge_Vectors.Vector;
   
   type Tree is record
      nodes   : Adjacency_List;
      n_leaves : Natural;
   end record;
   
   -- Read tree from input
   procedure Read_Tree(tree : in out Tree) is
      line : Unbounded_String;
      n    : Natural;
   begin
      Get_Line(line);
      n := Natural'Value(To_String(line));
      tree.n_leaves := n;
      
      -- Initialize adjacency list
      tree.nodes := (others => (others => (to_node => 0, weight => 0.0)));
      
      -- Read edges
      while not End_Of_File loop
         begin
            Get_Line(line);
            if Length(line) = 0 then
               exit;
            end if;
            
            -- Parse edge
            declare
               pos1 : Natural := Index(line, ' ');
               pos2 : Natural := Index(line, ' ', pos1 + 1);
               from : Node_Id := Node_Id'Value(To_String(Slice(line, 1, pos1 - 1)));
               to   : Node_Id := Node_Id'Value(To_String(Slice(line, pos1 + 1, pos2 - 1)));
               w    : Weight  := Weight'Value(To_String(Slice(line, pos2 + 1, Length(line))));
            begin
               -- Add edge to adjacency list
               Edge_Vectors.Append(tree.nodes(from), (to_node => to, weight => w));
               Edge_Vectors.Append(tree.nodes(to), (to_node => from, weight => w));
            end;
         exception
            when others => null;
         end;
      end loop;
   end Read_Tree;
   
   -- DFS to find distance between two nodes
   function Find_Distance(tree : Tree; start, target : Node_Id) return Weight is
      type Visited_Array is array(Node_Id) of Boolean;
      visited : Visited_Array := (others => False);
      type Queue_Element is record
         node   : Node_Id;
         dist   : Weight;
      end record;
      
      type Queue is array(1..1000) of Queue_Element;
      queue : Queue;
      front : Natural := 1;
      rear  : Natural := 0;
      
      procedure Enqueue(node : Node_Id; distance : Weight) is
      begin
         rear := rear + 1;
         queue(rear) := (node => node, dist => distance);
      end Enqueue;
      
      function Dequeue return Queue_Element is
         result : Queue_Element;
      begin
         result := queue(front);
         front := front + 1;
         return result;
      end Dequeue;
      
      procedure BFS is
         current : Queue_Element;
         neighbors : Edge_Vectors.Vector;
      begin
         Enqueue(start, 0.0);
         visited(start) := True;
         
         while front <= rear loop
            current := Dequeue;
            
            if current.node = target then
               Put_Line("Found distance: " & Weight'Image(current.dist));
               return;
            end if;
            
            neighbors := tree.nodes(current.node);
            for i in 1..Edge_Vectors.Length(neighbors) loop
               declare
                  edge : Edge := Edge_Vectors.Element(neighbors, i);
               begin
                  if not visited(edge.to_node) then
                     visited(edge.to_node) := True;
                     Enqueue(edge.to_node, current.dist + edge.weight);
                  end if;
               end;
            end loop;
         end loop;
      end BFS;
   begin
      BFS;
      return 0.0;
   end Find_Distance;
   
   -- Compute all pairwise distances
   procedure Compute_All_Distances(tree : Tree) is
      distances : array(1..tree.n_leaves, 1..tree.n_leaves) of Weight;
   begin
      -- For each pair of leaves
      for i in 1..tree.n_leaves loop
         for j in 1..tree.n_leaves loop
            if i /= j then
               -- Find distance between leaves i and j
               distances(i, j) := Find_Distance(tree, i, j);
            else
               distances(i, j) := 0.0;
            end if;
         end loop;
      end loop;
      
      -- Output results
      for i in 1..tree.n_leaves loop
         for j in 1..tree.n_leaves loop
            Put(Weight'Image(distances(i, j)));
            Put(" ");
         end loop;
         New_Line;
      end loop;
   end Compute_All_Distances;
   
   tree : Tree;
   
begin
   Read_Tree(tree);
   Compute_All_Distances(tree);
end Compute_Distances_Between_Leaves;
```

## Explanation

This Ada solution for the "Compute Distances Between Leaves" problem:

1. **Data Structures**: 
   - Uses a tree representation with adjacency lists
   - Defines `Node_Id` and `Weight` types for clarity
   - Implements a vector-based adjacency list structure

2. **Input Processing**:
   - Reads the number of leaves
   - Parses edges from input, building the adjacency list representation
   - Handles weighted edges between nodes

3. **Distance Calculation**:
   - Uses BFS (Breadth-First Search) to find shortest paths between leaves
   - Implements a queue-based traversal to compute distances
   - Maintains visited nodes to avoid cycles

4. **Output**:
   - Computes all pairwise distances between leaves
   - Outputs the distance matrix in the required format

## Key Features

- **Type Safety**: Uses Ada's strong typing system with custom types
- **Memory Management**: Uses Ada containers for dynamic arrays
- **Error Handling**: Includes exception handling for input parsing
- **Modularity**: Separates concerns into procedures for reading, calculating, and outputting

The solution handles the weighted tree structure and efficiently computes all pairwise distances between leaves using graph traversal algorithms.

