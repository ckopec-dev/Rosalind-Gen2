# Rosalind Problem: Construct a Trie from a Collection of Patterns (Ada Solution)

## Problem Understanding

The task is to construct a trie (prefix tree) from a collection of DNA patterns. A trie is a tree-like data structure where each path from root to leaf represents a pattern, and nodes represent nucleotides.

## Solution Approach

I'll implement a trie using Ada with:
1. A node-based structure for the trie
2. Methods to insert patterns into the trie
3. Output format matching Rosalind requirements

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Construct_A_Trie_From_Patterns is
   
   type Node_Ptr is access Node_Type;
   
   type Node_Type is record
      Children : array ('A' .. 'Z') of Node_Ptr := (others => null);
      Is_Leaf  : Boolean := False;
      Node_Id  : Natural := 0;
   end record;
   
   -- Global variables for node numbering and root
   Root_Node : Node_Ptr;
   Next_Id   : Natural := 1;
   
   procedure Insert_Pattern(Pattern : Unbounded_String);
   
   procedure Print_Trie(Node : Node_Ptr; Parent_Id : Natural);
   
   procedure Insert_Pattern(Pattern : Unbounded_String) is
      Current_Node : Node_Ptr := Root_Node;
      Char         : Character;
   begin
      -- Handle empty pattern
      if Length(Pattern) = 0 then
         return;
      end if;
      
      -- Traverse the trie, creating new nodes as needed
      for I in 1 .. Length(Pattern) loop
         Char := Element(Pattern, I);
         
         -- If character not in children, create new node
         if Current_Node.Children(Char) = null then
            Current_Node.Children(Char) := new Node_Type;
            Current_Node.Children(Char).Node_Id := Next_Id;
            Next_Id := Next_Id + 1;
         end if;
         
         -- Move to next node
         Current_Node := Current_Node.Children(Char);
      end loop;
      
      -- Mark the final node as a leaf
      Current_Node.Is_Leaf := True;
   end Insert_Pattern;
   
   procedure Print_Trie(Node : Node_Ptr; Parent_Id : Natural) is
      Child_Id : Natural;
   begin
      if Node = null then
         return;
      end if;
      
      -- For each child that exists, print the edge and recurse
      for Char in 'A' .. 'Z' loop
         if Node.Children(Char) /= null then
            Child_Id := Node.Children(Char).Node_Id;
            
            -- Print edge from parent to child
            Put(Parent_Id, Width => 0);
            Put("->");
            Put(Child_Id, Width => 0);
            Put(" ");
            Put_Line(Char);
            
            -- Recursively print children
            Print_Trie(Node.Children(Char), Child_Id);
         end if;
      end loop;
   end Print_Trie;
   
   -- Main program logic
   Pattern_Count : Natural;
   Input_Pattern : Unbounded_String;
begin
   -- Read number of patterns
   Get(Pattern_Count);
   Skip_Line;
   
   -- Initialize root node
   Root_Node := new Node_Type;
   Root_Node.Node_Id := Next_Id;
   Next_Id := Next_Id + 1;
   
   -- Read and insert all patterns
   for I in 1 .. Pattern_Count loop
      Get_Line(Input_Pattern);
      Insert_Pattern(Input_Pattern);
   end loop;
   
   -- Print the trie
   Print_Trie(Root_Node, Root_Node.Node_Id);
   
end Construct_A_Trie_From_Patterns;
```

## Explanation

### Data Structure
- **Node_Type**: Represents each node in the trie with:
  - `Children`: Array of pointers to child nodes for each nucleotide ('A' through 'Z')
  - `Is_Leaf`: Boolean flag to mark end of a pattern
  - `Node_Id`: Unique identifier for each node

### Key Functions
1. **Insert_Pattern**: Builds the trie by traversing existing paths or creating new nodes
2. **Print_Trie**: Recursively outputs edges in the required format (parent->child character)

### Output Format
The program outputs edges in the format:
```
parent_id->child_id character
```

Where each line represents an edge in the trie, with the character of the edge labeled on that line.

## Usage Example

For input patterns: `ATAGA`, `ATC`, `GAT`
The output would represent the trie structure with appropriate node numbering and edges.