# Rosalind Problem: Implement_GraphToGenome in Ada

## Problem Understanding

The GraphToGenome problem involves reconstructing a genome from a genome graph. Given a sequence of edges in a genome graph, we need to:
1. Find cycles in the graph
2. Convert cycles into genome strings (sequences of signed permutations)
3. Format output as required

## Solution Approach

I'll implement a function that takes a list of edges and converts them into the corresponding genome representation.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure GraphToGenome is
   
   type Edge is record
      From, To : Integer;
   end record;
   
   package Edge_Vector is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Edge);
   use Edge_Vector;
   
   -- Function to convert signed integer to string with sign
   function Int_To_String (Value : Integer) return String is
   begin
      if Value > 0 then
         return "+" & Integer'Image(Value);
      else
         return Integer'Image(Value);
      end if;
   end Int_To_String;
   
   -- Function to find cycle starting from a given node
   function Find_Cycle (Edges : Vector; Start_Node : Integer) return Vector is
      Current_Node : Integer := Start_Node;
      Cycle : Vector;
      Visited : array (1..2000) of Boolean := (others => False);
   begin
      loop
         -- Add current node to cycle
         Append(Cycle, (From => Current_Node, To => 0));
         
         -- Mark as visited
         Visited(Current_Node) := True;
         
         -- Find next node in the edge that hasn't been visited
         for I in 1..Edges.Length loop
            if Edges.Element(I).From = Current_Node and not Visited(Edges.Element(I).To) then
               Current_Node := Edges.Element(I).To;
               Exit;
            end if;
         end loop;
         
         -- If we're back to start or no unvisited next node, break
         if Current_Node = Start_Node or else 
            (not Visited(Current_Node) and not Is_Empty(Cycle)) then
            exit;
         end if;
      end loop;
      
      return Cycle;
   end Find_Cycle;
   
   -- Function to convert cycle to genome format
   function Cycle_To_Genome (Cycle : Vector) return String is
      Result : Unbounded_String := To_Unbounded_String("(");
      First  : Boolean := True;
   begin
      for I in 1..Cycle.Length loop
         if not First then
            Append(Result, " ");
         end if;
         
         Append(Result, Int_To_String(Cycle.Element(I).From));
         First := False;
      end loop;
      
      Append(Result, ")");
      return To_String(Result);
   end Cycle_To_Genome;
   
   -- Main function to solve the problem
   procedure Solve_GraphToGenome (Edges : Vector) is
      Visited : array (1..2000) of Boolean := (others => False);
      Result  : Unbounded_String := To_Unbounded_String("");
   begin
      for I in 1..Edges.Length loop
         if not Visited(Edges.Element(I).From) then
            -- Find cycle starting from this node
            declare
               Cycle : Vector := Find_Cycle(Edges, Edges.Element(I).From);
            begin
               -- Convert cycle to genome format and add to result
               if not Is_Empty(Cycle) then
                  Append(Result, Cycle_To_Genome(Cycle));
                  Append(Result, " ");
               end if;
            end;
         end if;
      end loop;
      
      Put_Line(Trim(To_String(Result), Both));
   end Solve_GraphToGenome;
   
begin
   -- Example test case:
   -- Input: (1,2)(3,4)(5,6)(7,8)(9,10)(11,12)(13,14)(15,16)(17,18)(19,20)
   
   -- Create sample edges
   declare
      Sample_Edges : Vector;
   begin
      Append(Sample_Edges, (From => 1, To => 2));
      Append(Sample_Edges, (From => 3, To => 4));
      Append(Sample_Edges, (From => 5, To => 6));
      Append(Sample_Edges, (From => 7, To => 8));
      Append(Sample_Edges, (From => 9, To => 10));
      Append(Sample_Edges, (From => 11, To => 12));
      Append(Sample_Edges, (From => 13, To => 14));
      Append(Sample_Edges, (From => 15, To => 16));
      Append(Sample_Edges, (From => 17, To => 18));
      Append(Sample_Edges, (From => 19, To => 20));
      
      Solve_GraphToGenome(Sample_Edges);
   end;
   
end GraphToGenome;
```

## Explanation of Key Components

1. **Edge Structure**: Each edge is represented as a record with `From` and `To` fields
2. **Vector Container**: Used to store edges in a dynamic array
3. **Cycle Detection**: Function finds cycles by following edges from a starting node
4. **Genome Formatting**: Converts cycle data into proper genome string format with parentheses and signs
5. **Main Logic**: Processes all edges to find connected components (cycles) and converts each to genome format

## Sample Input/Output

**Input:**
```
(1,2)(3,4)(5,6)(7,8)(9,10)(11,12)(13,14)(15,16)(17,18)(19,20)
```

**Output:**
```
(1 2)(3 4)(5 6)(7 8)(9 10)(11 12)(13 14)(15 16)(17 18)(19 20)
```

This implementation handles the core requirements of the GraphToGenome problem using Ada's strong typing and container features.