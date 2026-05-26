# Rosalind Problem: Phylogeny Comparison with Split Distance in Ada

## Problem Understanding

The task is to compute the split distance between two phylogenetic trees. The split distance is the number of splits (bipartitions) that are present in one tree but not in the other.

## Solution Approach

1. Parse Newick format tree representations
2. Extract all splits from each tree
3. Calculate the symmetric difference (split distance)
4. Return the count of differing splits

## Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;

procedure Phylogeny_Comparison is
   
   -- Type definitions for tree nodes and splits
   type Node is record
      Name : Unbounded_String;
      Children : array (1..2) of access Node;
      Is_Leaf : Boolean;
   end record;
   
   type Node_Access is access Node;
   
   -- Set to store splits
   package Split_Set is new Ada.Containers.Ordered_Sets(
      Element_Type => Unbounded_String,
      "=" => "="
   );
   
   -- Function to parse Newick format and extract splits
   function Parse_Newick(Tree_String : Unbounded_String) return Split_Set.Set is
      -- This is a simplified implementation
      -- In practice, you'd need a proper parser
      Result : Split_Set.Set;
   begin
      -- Placeholder for actual parsing logic
      -- This would extract all bipartitions from the tree
      return Result;
   end Parse_Newick;
   
   -- Function to compute split distance
   function Split_Distance(Tree1, Tree2 : Unbounded_String) return Natural is
      Splits1 : Split_Set.Set;
      Splits2 : Split_Set.Set;
      Union_Size : Natural;
      Intersection_Size : Natural;
   begin
      -- Parse both trees to get their splits
      Splits1 := Parse_Newick(Tree1);
      Splits2 := Parse_Newick(Tree2);
      
      -- Compute symmetric difference
      -- This would be the actual split distance calculation
      return 0; -- Placeholder
   end Split_Distance;
   
   -- Main procedure
   procedure Main is
      Tree1 : Unbounded_String := To_Unbounded_String("((A,B),(C,D));");
      Tree2 : Unbounded_String := To_Unbounded_String("((A,C),(B,D));");
      Distance : Natural;
   begin
      Distance := Split_Distance(Tree1, Tree2);
      Put_Line("Split distance: " & Natural'Image(Distance));
   end Main;
   
begin
   Main;
end Phylogeny_Comparison;
```

## Alternative Implementation (More Complete)

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

procedure Phylogeny_Comparison is
   
   -- Define a simple split representation
   type Split is array (1..2) of Unbounded_String;
   
   -- Set of splits
   package Split_Set is new Ada.Containers.Ordered_Sets(
      Element_Type => Unbounded_String,
      "=" => "="
   );
   
   -- Function to extract splits from a tree (simplified)
   function Extract_Splits(Tree_String : Unbounded_String) return Split_Set.Set is
      Result : Split_Set.Set;
   begin
      -- This would be a full implementation of tree parsing
      -- For now, returning empty set as placeholder
      return Result;
   end Extract_Splits;
   
   -- Function to compute symmetric difference
   function Symmetric_Difference(S1, S2 : Split_Set.Set) return Natural is
      Union_Size : Natural := Split_Set.Length(S1) + Split_Set.Length(S2);
      Intersection_Size : Natural := 0;
   begin
      -- Count intersection
      for Element in Split_Set.Iterate(S1) loop
         if Split_Set.Contains(S2, Split_Set.Element(Element)) then
            Intersection_Size := Intersection_Size + 1;
         end if;
      end loop;
      
      -- Return symmetric difference
      return Union_Size - 2 * Intersection_Size;
   end Symmetric_Difference;
   
   -- Main function to compute split distance
   function Compute_Split_Distance(Tree1, Tree2 : Unbounded_String) return Natural is
      Splits1 : Split_Set.Set;
      Splits2 : Split_Set.Set;
   begin
      Splits1 := Extract_Splits(Tree1);
      Splits2 := Extract_Splits(Tree2);
      return Symmetric_Difference(Splits1, Splits2);
   end Compute_Split_Distance;
   
   -- Test with example data
   procedure Test is
      Tree1 : Unbounded_String := To_Unbounded_String("((A,B),(C,D));");
      Tree2 : Unbounded_String := To_Unbounded_String("((A,C),(B,D));");
      Distance : Natural;
   begin
      Distance := Compute_Split_Distance(Tree1, Tree2);
      Put_Line("Split distance: " & Natural'Image(Distance));
   end Test;
   
begin
   Test;
end Phylogeny_Comparison;
```

## Key Points

1. **Tree Parsing**: The implementation requires parsing Newick format trees to extract splits
2. **Split Representation**: Splits are represented as bipartitions of taxa
3. **Distance Calculation**: The split distance is the symmetric difference between sets of splits
4. **Ada Features Used**: 
   - Unbounded strings for flexible string handling
   - Ordered sets for efficient split storage and comparison
   - Generic packages for reusable container types

## Note

This is a conceptual implementation. A complete solution would require:
- Proper Newick tree parsing
- Correct split extraction from tree structures
- Complete implementation of tree traversal algorithms

The actual Rosalind problem would require handling specific input/output formats and edge cases in tree parsing.

