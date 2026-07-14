# Rosalind Problem: Negative Weight Cycle (Ada Solution)

## Problem Understanding

The negative weight cycle problem asks us to detect whether a weighted directed graph contains a negative weight cycle reachable from a specified source node.

## Approach

I'll use the Bellman-Ford algorithm modified to detect negative cycles:
1. Run Bellman-Ford for V-1 iterations
2. Perform one more iteration to check if any distances can be improved
3. If improvements are possible, a negative cycle exists

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Negative_Weight_Cycle is
   type Edge is record
      Source : Integer;
      Target : Integer;
      Weight : Float;
   end record;
   
   type Edge_Array is array (Positive range <>) of Edge;
   
   type Distance_Array is array (Positive range <>) of Float;
   
   -- Read input
   procedure Read_Input(N : in out Integer; M : in out Integer) is
      Temp : Integer;
   begin
      Get(N);
      Get(M);
      Skip_Line;
   end Read_Input;
   
   -- Bellman-Ford algorithm to detect negative cycle
   function Has_Negative_Cycle(Edges : in Edge_Array; 
                              N : in Integer; 
                              Source : in Integer) return Boolean is
      Distances : Distance_Array(1..N);
      Updated : Boolean;
      Changed : Boolean;
   begin
      -- Initialize distances
      for I in 1..N loop
         Distances(I) := Float'Last;
      end loop;
      Distances(Source) := 0.0;
      
      -- Relax edges V-1 times
      for Iteration in 1..N-1 loop
         Updated := False;
         for E in Edges'Range loop
            if Distances(E.Source) /= Float'Last then
               if Distances(E.Target) > Distances(E.Source) + E.Weight then
                  Distances(E.Target) := Distances(E.Source) + E.Weight;
                  Updated := True;
               end if;
            end if;
         end loop;
         
         -- Early termination if no updates
         exit when not Updated;
      end loop;
      
      -- Check for negative cycle in one more iteration
      Changed := False;
      for E in Edges'Range loop
         if Distances(E.Source) /= Float'Last then
            if Distances(E.Target) > Distances(E.Source) + E.Weight then
               Changed := True;
               exit;
            end if;
         end if;
      end loop;
      
      return Changed;
   end Has_Negative_Cycle;
   
   -- Main program
   N : Integer;
   M : Integer;
   Edges : Edge_Array(1..10000);  -- Assuming maximum edges
   Source : Integer := 1;
   Result : Boolean;
begin
   Read_Input(N, M);
   
   -- Read edges
   for I in 1..M loop
      Get(Edges(I).Source);
      Get(Edges(I).Target);
      Get(Edges(I).Weight);
      Skip_Line;
   end loop;
   
   -- Check for negative cycle
   Result := Has_Negative_Cycle(Edges, N, Source);
   
   -- Output result
   if Result then
      Put_Line("1");  -- Negative cycle exists
   else
      Put_Line("0");  -- No negative cycle
   end if;
end Negative_Weight_Cycle;
```

## Explanation

The solution implements the Bellman-Ford algorithm with a modification to detect negative cycles:

1. **Initialization**: Set all distances to infinity except the source which is set to 0
2. **Relaxation**: Run V-1 iterations of edge relaxation
3. **Cycle Detection**: Run one more iteration to check if any distance can be improved
4. **Result**: If improvements are possible, a negative cycle exists

## Key Features

- Uses proper Ada array declarations and bounds checking
- Handles floating-point weights as required by the problem
- Implements efficient early termination in Bellman-Ford
- Follows Rosalind's expected input/output format

## Time Complexity
O(V×E) where V is vertices and E is edges

## Space Complexity  
O(V+E) for storing graph and distance arrays

This solution correctly identifies negative weight cycles in directed graphs as required by the Rosalind problem.