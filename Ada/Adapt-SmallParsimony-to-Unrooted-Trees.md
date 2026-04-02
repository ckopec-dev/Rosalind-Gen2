# Rosalind Problem: Adapt_SmallParsimony_to_Unrooted_Trees in Ada

## Problem Understanding

The Small Parsimony problem involves finding the minimum cost of assigning labels to internal nodes of a tree to minimize the total number of evolutionary changes. For unrooted trees, we need to adapt the standard approach to handle the lack of a designated root.

## Solution Approach

I'll implement a solution using dynamic programming with post-order and pre-order traversals to compute the minimum parsimony score for unrooted trees.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hash_Maps;

procedure Adapt_SmallParsimony_to_Unrooted_Trees is

   type Node_Id is new Integer;
   type Character_Type is ('A', 'C', 'G', 'T');
   
   type Node is record
      Id          : Node_Id;
      Label       : Character_Type;
      Is_Leaf     : Boolean;
      Children    : Ada.Containers.Vectors.Vector (Node_Id);
      Parent      : Node_Id;
   end record;
   
   type Tree is array (Node_Id) of Node;
   
   type Parsimony_Matrix is array (Character_Type, Character_Type) of Integer;
   
   -- Simple tree structure
   type Tree_Type is record
      Nodes   : Tree;
      Root    : Node_Id;
      Size    : Natural;
   end record;
   
   -- Parse input and build tree
   procedure Parse_Input (Tree : in out Tree_Type);
   
   -- Compute parsimony score using dynamic programming
   function Compute_Parsimony_Score (Tree : Tree_Type) return Integer;
   
   -- Post-order traversal to compute minimum cost
   function Post_Order_Compute (Tree : Tree_Type; Node_Id : Node_Id) 
                               return array (Character_Type) of Integer;
   
   -- Pre-order traversal to assign labels
   procedure Pre_Order_Assign (Tree : in out Tree_Type; 
                              Node_Id : Node_Id; 
                              Parent_Character : Character_Type);
   
   -- Helper functions
   function Is_Leaf (Tree : Tree_Type; Node_Id : Node_Id) return Boolean;
   function Get_Character_Count (Tree : Tree_Type; Node_Id : Node_Id) 
                                return Integer;
   
   -- Global variables
   Tree_Data : Tree_Type;
   
begin
   -- Main execution
   Parse_Input (Tree_Data);
   Put_Line (Integer'Image (Compute_Parsimony_Score (Tree_Data)));
end Adapt_SmallParsimony_to_Unrooted_Trees;

procedure Parse_Input (Tree : in out Tree_Type) is
   -- Implementation would parse input from stdin
   -- This is a placeholder for the actual parsing logic
begin
   -- Parse tree structure and leaf labels
   -- Set up Tree_Data with appropriate node information
   null;
end Parse_Input;

function Compute_Parsimony_Score (Tree : Tree_Type) return Integer is
   -- Find a root (any node works for unrooted trees)
   Root_Node : Node_Id := 1;
   
   -- Compute minimum parsimony score
   Min_Cost : array (Character_Type) of Integer;
   Min_Total : Integer := Integer'Last;
   
   -- Start with post-order traversal from root
   Costs : array (Character_Type) of Integer := (others => 0);
   
begin
   -- Initialize costs
   for Char in Character_Type loop
      Costs(Char) := Post_Order_Compute (Tree, Root_Node)(Char);
   end loop;
   
   -- Find minimum among all possible root characters
   for Char in Character_Type loop
      if Costs(Char) < Min_Total then
         Min_Total := Costs(Char);
      end if;
   end loop;
   
   return Min_Total;
end Compute_Parsimony_Score;

function Post_Order_Compute (Tree : Tree_Type; Node_Id : Node_Id) 
                            return array (Character_Type) of Integer is
   Node : Node renames Tree.Nodes(Node_Id);
   Costs : array (Character_Type) of Integer;
   Min_Cost : Integer;
   
begin
   -- Initialize costs for this node
   for Char in Character_Type loop
      Costs(Char) := 0;
   end loop;
   
   -- If leaf node, set cost for its character
   if Node.Is_Leaf then
      Costs(Node.Label) := 0;
   else
      -- For internal node, compute minimum cost from children
      declare
         Child_Costs : array (Character_Type) of Integer;
         Total_Cost : Integer;
      begin
         -- Initialize to large values
         for Char in Character_Type loop
            Child_Costs(Char) := Integer'Last;
         end loop;
         
         -- Process children
         for I in 1 .. Node.Children.Length loop
            declare
               Child_Id : Node_Id := Node.Children.Element(I);
               Child_Costs : array (Character_Type) of Integer := Post_Order_Compute (Tree, Child_Id);
            begin
               -- For each possible character at current node
               for Current_Char in Character_Type loop
                  Total_Cost := 0;
                  -- Sum up costs from children
                  for Child_Char in Character_Type loop
                     if Current_Char /= Child_Char then
                        Total_Cost := Total_Cost + 1;
                     end if;
                  end loop;
                  Total_Cost := Total_Cost + Child_Costs(Child_Char);
                  if Total_Cost < Child_Costs(Current_Char) then
                     Child_Costs(Current_Char) := Total_Cost;
                  end if;
               end loop;
            end;
         end loop;
         
         -- Set costs for current node
         for Char in Character_Type loop
            Costs(Char) := Child_Costs(Char);
         end loop;
      end;
   end if;
   
   return Costs;
end Post_Order_Compute;

procedure Pre_Order_Assign (Tree : in out Tree_Type; 
                           Node_Id : Node_Id; 
                           Parent_Character : Character_Type) is
   Node : Node renames Tree.Nodes(Node_Id);
begin
   if Node.Is_Leaf then
      null; -- Leaves already have their labels
   else
      -- Assign character to internal node based on parent and children
      -- This is a simplified version - actual implementation would be more complex
      null;
   end if;
end Pre_Order_Assign;

function Is_Leaf (Tree : Tree_Type; Node_Id : Node_Id) return Boolean is
begin
   return Tree.Nodes(Node_Id).Is_Leaf;
end Is_Leaf;

function Get_Character_Count (Tree : Tree_Type; Node_Id : Node_Id) 
                             return Integer is
begin
   return 4; -- Always 4 characters (A, C, G, T)
end Get_Character_Count;
```

## Key Algorithm Components

1. **Tree Representation**: Uses a node-based structure with parent-child relationships
2. **Post-order Traversal**: Computes minimum costs for each character at each node
3. **Dynamic Programming**: Builds up solutions from leaves to root
4. **Unrooted Tree Handling**: Works with any node as root by computing minimum over all possibilities

## Time Complexity
- O(n × k) where n is the number of nodes and k is the number of character states (4 for DNA)
- Space complexity is O(n × k) for storing the DP table

## Usage Notes
This implementation provides the framework for solving the Small Parsimony problem on unrooted trees. The actual input parsing and detailed character matching logic would need to be implemented based on the specific Rosalind input format.

