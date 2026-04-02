# Rosalind Problem: Implement_SmallParsimony in Ada

## Problem Understanding

Small Parsimony is a problem in phylogenetics where we need to find the minimum number of evolutionary changes required to explain the observed character states at the leaves of a tree. Given a tree structure and character states at the leaves, we want to assign states to internal nodes to minimize the total number of changes.

## Solution Approach

We'll implement the Small Parsimony algorithm using dynamic programming with the Fitch algorithm approach:

1. **Post-order traversal**: Calculate minimum costs for each node
2. **Pre-order traversal**: Assign optimal states to internal nodes
3. **Calculate total parsimony score**

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Implement_SmallParsimony is
   
   -- Define constants
   type Character_Type is ('A', 'C', 'G', 'T');
   type Node_Id is range 0..1000;
   type State_Set is array (Character_Type) of Boolean;
   
   -- Tree node structure
   type Node is record
      Id         : Node_Id;
      Is_Leaf    : Boolean;
      Label      : Unbounded_String;
      Children   : array (1..2) of Node_Id;
      Child_Count : Natural := 0;
      State      : State_Set;
      Cost       : array (Character_Type) of Natural;
   end record;
   
   -- Tree structure
   type Tree is array (Node_Id) of Node;
   
   -- Global variables
   Tree_Data : Tree;
   Num_Nodes : Natural := 0;
   Num_Chars : Natural := 0;
   
   -- Function to convert character to index
   function Char_To_Index(C : Character_Type) return Natural is
   begin
      case C is
         when 'A' => return 0;
         when 'C' => return 1;
         when 'G' => return 2;
         when 'T' => return 3;
      end case;
   end Char_To_Index;
   
   -- Function to convert index to character
   function Index_To_Char(Index : Natural) return Character_Type is
   begin
      case Index is
         when 0 => return 'A';
         when 1 => return 'C';
         when 2 => return 'G';
         when 3 => return 'T';
         when others => return 'A';
      end case;
   end Index_To_Char;
   
   -- Initialize node
   procedure Initialize_Node(N : in out Node; Id : Node_Id; Is_Leaf : Boolean) is
   begin
      N.Id := Id;
      N.Is_Leaf := Is_Leaf;
      N.Child_Count := 0;
      for I in Character_Type loop
         N.State(I) := False;
         N.Cost(I) := 0;
      end loop;
   end Initialize_Node;
   
   -- Add child to node
   procedure Add_Child(N : in out Node; Child_Id : Node_Id) is
   begin
      if N.Child_Count < 2 then
         N.Child_Count := N.Child_Count + 1;
         N.Children(N.Child_Count) := Child_Id;
      end if;
   end Add_Child;
   
   -- Get minimum cost character
   function Get_Min_Cost(Costs : array of Natural) return Character_Type is
      Min_Cost : Natural := Natural'Last;
      Min_Char : Character_Type := 'A';
   begin
      for I in Costs'Range loop
         if Costs(I) < Min_Cost then
            Min_Cost := Costs(I);
            Min_Char := Index_To_Char(I);
         end if;
      end loop;
      return Min_Char;
   end Get_Min_Cost;
   
   -- Get minimum cost value
   function Get_Min_Cost_Value(Costs : array of Natural) return Natural is
      Min_Cost : Natural := Natural'Last;
   begin
      for I in Costs'Range loop
         if Costs(I) < Min_Cost then
            Min_Cost := Costs(I);
         end if;
      end loop;
      return Min_Cost;
   end Get_Min_Cost_Value;
   
   -- Post-order traversal to calculate costs
   procedure Post_Order_Traversal(Node_Id : Node_Id) is
      Node : Node renames Tree_Data(Node_Id);
   begin
      if Node.Is_Leaf then
         -- For leaf nodes, initialize costs based on character
         for C in Character_Type loop
            if Node.State(C) then
               Node.Cost(C) := 0;
            else
               Node.Cost(C) := 1;
            end if;
         end loop;
      else
         -- Recursively process children
         for I in 1..Node.Child_Count loop
            Post_Order_Traversal(Node.Children(I));
         end loop;
         
         -- Calculate costs for internal node
         for C in Character_Type loop
            Node.Cost(C) := 0;
            for I in 1..Node.Child_Count loop
               declare
                  Child_Id : Node_Id := Node.Children(I);
                  Child_Node : Node renames Tree_Data(Child_Id);
                  Min_Cost : Natural := Natural'Last;
               begin
                  for J in Character_Type loop
                     if Child_Node.Cost(J) < Min_Cost then
                        Min_Cost := Child_Node.Cost(J);
                     end if;
                  end loop;
                  Node.Cost(C) := Node.Cost(C) + Min_Cost;
               end;
            end loop;
         end loop;
      end if;
   end Post_Order_Traversal;
   
   -- Pre-order traversal to assign states
   procedure Pre_Order_Traversal(Node_Id : Node_Id; Parent_State : State_Set) is
      Node : Node renames Tree_Data(Node_Id);
   begin
      if Node.Is_Leaf then
         -- Leaf nodes already have their states assigned
         null;
      else
         -- Assign states based on children's states
         declare
            Assigned_States : State_Set;
            Min_Total_Cost : Natural := Natural'Last;
            Best_Char : Character_Type := 'A';
         begin
            -- For each possible character at current node
            for C in Character_Type loop
               declare
                  Total_Cost : Natural := 0;
                  Valid : Boolean := True;
               begin
                  -- Check if this character is compatible with children
                  for I in 1..Node.Child_Count loop
                     declare
                        Child_Id : Node_Id := Node.Children(I);
                        Child_Node : Node renames Tree_Data(Child_Id);
                        Child_Cost : Natural := Natural'Last;
                     begin
                        -- Find minimum cost for child node
                        for J in Character_Type loop
                           if Child_Node.Cost(J) < Child_Cost then
                              Child_Cost := Child_Node.Cost(J);
                           end if;
                        end loop;
                        Total_Cost := Total_Cost + Child_Cost;
                     end;
                  end loop;
                  
                  if Total_Cost < Min_Total_Cost then
                     Min_Total_Cost := Total_Cost;
                     Best_Char := C;
                  end if;
               end;
            end loop;
            
            -- Assign best character to node
            Node.State(Best_Char) := True;
            
            -- Propagate to children
            for I in 1..Node.Child_Count loop
               Pre_Order_Traversal(Node.Children(I), Node.State);
            end loop;
         end;
      end if;
   end Pre_Order_Traversal;
   
   -- Main function to solve small parsimony
   procedure Solve_Small_Parsimony is
      Total_Cost : Natural := 0;
   begin
      -- Post-order traversal to calculate costs
      Post_Order_Traversal(0); -- Assuming root is at index 0
      
      -- Pre-order traversal to assign states
      Pre_Order_Traversal(0, (others => False));
      
      -- Calculate total parsimony score
      for I in 0..Num_Nodes-1 loop
         declare
            Node : Node renames Tree_Data(I);
         begin
            if Node.Is_Leaf then
               null; -- Leaf nodes don't contribute to cost
            else
               Total_Cost := Total_Cost + Get_Min_Cost_Value(Node.Cost);
            end if;
         end;
      end loop;
      
      Put_Line("Total Parsimony Score: " & Total_Cost'Image);
   end Solve_Small_Parsimony;
   
   -- Read input function
   procedure Read_Input is
      Line : Unbounded_String;
      Num_Leaves : Natural;
   begin
      -- Read number of leaves
      Get_Line(Line);
      Num_Leaves := Natural'Value(To_String(Line));
      
      -- Read leaf data and build tree
      for I in 1..Num_Leaves loop
         Get_Line(Line);
         -- Process leaf data
      end loop;
      
      -- Read tree structure
      -- This is a simplified version - in practice you'd parse the tree structure
   end Read_Input;
   
begin
   -- Initialize tree with sample data
   Num_Nodes := 4;
   Num_Chars := 4;
   
   -- Initialize root node
   Initialize_Node(Tree_Data(0), 0, False);
   
   -- Initialize leaf nodes
   for I in 1..3 loop
      Initialize_Node(Tree_Data(I), I, True);
   end loop;
   
   -- Build tree structure
   -- Add children to root
   Add_Child(Tree_Data(0), 1);
   Add_Child(Tree_Data(0), 2);
   Add_Child(Tree_Data(1), 3);
   
   -- Set leaf states (example)
   Tree_Data(1).State('A') := True;
   Tree_Data(2).State('C') := True;
   Tree_Data(3).State('G') := True;
   
   -- Solve the problem
   Solve_Small_Parsimony;
   
end Implement_Small_Parsimony;
```

## Key Features of the Implementation

1. **Tree Structure**: Uses an array-based representation with nodes having child relationships
2. **Dynamic Programming**: Implements the Fitch algorithm with post-order and pre-order traversals
3. **State Management**: Properly handles character states for both leaves and internal nodes
4. **Cost Calculation**: Computes minimum parsimony scores efficiently
5. **Ada-Specific Features**: Uses Ada's strong typing, arrays, and text I/O capabilities

## Algorithm Steps

1. **Post-order traversal**: Calculate minimum costs for each node based on child nodes
2. **Pre-order traversal**: Assign optimal character states to internal nodes
3. **Cost aggregation**: Sum up all minimum costs to get the total parsimony score

## Time Complexity

- **Time**: O(N × M) where N is the number of nodes and M is the number of characters
- **Space**: O(N × M) for storing the tree and cost information

This implementation provides a foundation for solving the Small Parsimony problem in the Rosalind dataset using Ada programming language.

