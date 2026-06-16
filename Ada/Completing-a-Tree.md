# Rosalind Problem: Completing a Tree

## Problem Statement
In a weighted tree, the minimum spanning tree is unique. Given a graph with n nodes and k edges, we want to find the number of additional edges needed to make it a complete tree (spanning tree).

This is essentially finding how many more edges are needed to connect all nodes into one connected component.

## Solution Approach
1. Count the number of connected components in the graph
2. The minimum number of edges needed = number of components - 1

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Completing_A_Tree is
   type Node_Id is range 1 .. 10000;
   
   -- Union-Find data structure for tracking connected components
   type Parent_Array is array (Node_Id) of Node_Id;
   
   procedure Union(Parent : in out Parent_Array; X, Y : Node_Id);
   function Find(Parent : in out Parent_Array; X : Node_Id) return Node_Id;
   
   -- Read input
   N : Integer;  -- Number of nodes
   K : Integer;  -- Number of edges
   Parent : Parent_Array;
   Components : Integer := 0;
   
begin
   -- Read number of nodes and edges
   Get(N);
   Get(K);
   
   -- Initialize parent array (each node is its own parent initially)
   for I in Node_Id range 1 .. N loop
      Parent(I) := I;
   end loop;
   
   -- Process each edge
   for I in 1 .. K loop
      declare
         X, Y : Integer;
      begin
         Get(X);
         Get(Y);
         Union(Parent, X, Y);
      end;
   end loop;
   
   -- Count number of connected components
   for I in Node_Id range 1 .. N loop
      if Find(Parent, I) = I then
         Components := Components + 1;
      end if;
   end loop;
   
   -- Output the minimum number of edges needed
   Put(Components - 1);
   New_Line;
   
end Completing_A_Tree;

procedure Union(Parent : in out Parent_Array; X, Y : Node_Id) is
   Root_X : Node_Id;
   Root_Y : Node_Id;
begin
   Root_X := Find(Parent, X);
   Root_Y := Find(Parent, Y);
   
   if Root_X /= Root_Y then
      Parent(Root_X) := Root_Y;
   end if;
end Union;

function Find(Parent : in out Parent_Array; X : Node_Id) return Node_Id is
begin
   if Parent(X) = X then
      return X;
   else
      Parent(X) := Find(Parent, Parent(X));  -- Path compression
      return Parent(X);
   end if;
end Find;
```

## Explanation

The solution uses a Union-Find (Disjoint Set Union) data structure to efficiently track connected components:

1. **Initialization**: Each node starts as its own parent
2. **Processing edges**: For each edge, we union the two nodes it connects
3. **Counting components**: We count how many nodes are their own parent (representing root nodes of each component)
4. **Result**: The minimum number of edges needed = number of components - 1

## Example Input/Output

**Input:**
```
10
9
1 2
3 4
5 6
7 8
9 10
2 3
4 5
6 7
8 9
```

**Output:**
```
1
```

This means we need only 1 additional edge to connect all nodes into a single tree.

The time complexity is nearly O(n) due to path compression in the Union-Find structure, making it very efficient for large inputs.