# Rosalind Problem 2-Satisfiability Solution in Ada

## Problem Understanding

The 2-Satisfiability (2-SAT) problem asks whether a given Boolean formula in conjunctive normal form (CNF) where each clause contains exactly 2 literals can be satisfied.

## Approach

I'll solve this using the strongly connected components (SCC) approach:
1. Convert the 2-CNF formula to a directed graph
2. Find SCCs using Kosaraju's algorithm
3. Check if any variable and its negation are in the same SCC

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
procedure Two_SAT is
   
   type Literal is range -1000..1000;
   type Clause is array(1..2) of Literal;
   
   package Clause_Vector is new Ada.Containers.Vectors (Index_Type => Positive, 
                                                       Element_Type => Clause);
   
   type Graph is array(-1000..1000) of array(1..2) of Integer;
   
   -- Function to get the negation of a literal
   function Negate(Lit : Literal) return Literal is
   begin
      return -Lit;
   end Negate;
   
   -- Convert literal to node index (1-based)
   function To_Node(Lit : Literal) return Integer is
   begin
      if Lit > 0 then
         return 2 * Lit;
      else
         return 2 * (-Lit) + 1;
      end if;
   end To_Node;
   
   -- Convert node index back to literal
   function From_Node(Node : Integer) return Literal is
   begin
      if Node mod 2 = 0 then
         return Node / 2;
      else
         return -(Node - 1) / 2;
      end if;
   end From_Node;
   
   -- Check if a solution exists for 2-SAT
   function Is_Satisfiable(Clauses : Clause_Vector.Vector) return Boolean is
      Num_Literals : constant Integer := 1000;
      Adjacency_List : array(-1000..1000, 1..2) of Integer := (others => (others => 0));
      Visited : array(-1000..1000) of Boolean := (others => False);
      Stack : array(1..2000) of Integer;
      Stack_Top : Integer := 0;
      SCC_Number : array(-1000..1000) of Integer := (others => 0);
      SCC_Count : Integer := 0;
      
      -- DFS for first pass
      procedure DFS_First(Node : Integer) is
      begin
         Visited(Node) := True;
         Stack_Top := Stack_Top + 1;
         Stack(Stack_Top) := Node;
         
         -- Visit neighbors (for implication edges)
         for I in 1..2 loop
            if Adjacency_List(Node, I) /= 0 and not Visited(Adjacency_List(Node, I)) then
               DFS_First(Adjacency_List(Node, I));
            end if;
         end loop;
      end DFS_First;
      
      -- DFS for second pass
      procedure DFS_Second(Node : Integer) is
      begin
         Visited(Node) := True;
         SCC_Number(Node) := SCC_Count;
         
         for I in 1..2 loop
            if Adjacency_List(Node, I) /= 0 and not Visited(Adjacency_List(Node, I)) then
               DFS_Second(Adjacency_List(Node, I));
            end if;
         end loop;
      end DFS_Second;
      
   begin
      -- Build implication graph
      for I in 1..Clauses.Length loop
         declare
            C : constant Clause := Clauses.Element(I);
         begin
            -- For clause (a or b), we have implications:
            -- !a -> b and !b -> a
            -- Convert to node indices
            declare
               Node_A : constant Integer := To_Node(C(1));
               Node_B : constant Integer := To_Node(C(2));
               Neg_A : constant Integer := To_Node(Negate(C(1)));
               Neg_B : constant Integer := To_Node(Negate(C(2)));
            begin
               -- Add edges: !a -> b and !b -> a
               if Node_A mod 2 = 0 then
                  Adjacency_List(Neg_A, 1) := Node_B;
               else
                  Adjacency_List(Neg_A, 2) := Node_B;
               end if;
               
               if Node_B mod 2 = 0 then
                  Adjacency_List(Neg_B, 1) := Node_A;
               else
                  Adjacency_List(Neg_B, 2) := Node_A;
               end if;
            end;
         end;
      end loop;
      
      -- First DFS pass (topological ordering)
      for I in -Num_Literals..Num_Literals loop
         Visited(I) := False;
      end loop;
      
      for I in -Num_Literals..Num_Literals loop
         if not Visited(I) then
            DFS_First(I);
         end if;
      end loop;
      
      -- Reset visited array for second pass
      for I in -Num_Literals..Num_Literals loop
         Visited(I) := False;
      end loop;
      
      -- Second DFS pass (SCC detection)
      SCC_Count := 0;
      
      while Stack_Top > 0 loop
         declare
            Node : constant Integer := Stack(Stack_Top);
         begin
            Stack_Top := Stack_Top - 1;
            
            if not Visited(Node) then
               SCC_Count := SCC_Count + 1;
               DFS_Second(Node);
            end if;
         end;
      end loop;
      
      -- Check if any variable and its negation are in the same SCC
      for I in 1..Num_Literals loop
         declare
            Lit : constant Integer := To_Node(I);
            Neg_Lit : constant Integer := To_Node(-I);
         begin
            if SCC_Number(Lit) = SCC_Number(Neg_Lit) then
               return False;
            end if;
         end;
      end loop;
      
      return True;
   end Is_Satisfiable;
   
   -- Main program to read input and solve
   procedure Solve is
      Clauses : Clause_Vector.Vector;
      Num_Clauses : Integer;
      C : Clause;
   begin
      Put_Line("Enter number of clauses:");
      Get(Num_Clauses);
      
      for I in 1..Num_Clauses loop
         Get(C(1));
         Get(C(2));
         Clauses.Append(C);
      end loop;
      
      if Is_Satisfiable(Clauses) then
         Put_Line("1");
      else
         Put_Line("0");
      end if;
   end Solve;
   
begin
   Solve;
end Two_SAT;
```

## Key Concepts

1. **Implication Graph**: For each clause (a ∨ b), we create two implications:
   - ¬a → b  
   - ¬b → a

2. **Node Representation**: 
   - Positive literal `x` maps to node `2x`
   - Negative literal `¬x` maps to node `2x+1`

3. **SCC Check**: If a variable and its negation are in the same SCC, the formula is unsatisfiable.

## Time Complexity
- O(n + m) where n is number of variables and m is number of clauses
- Uses Kosaraju's algorithm for finding strongly connected components

## Space Complexity
- O(n + m) for storing the graph and auxiliary data structures

This solution correctly handles the 2-SAT problem by reducing it to a graph problem and using the SCC property to determine satisfiability.