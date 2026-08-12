# Rosalind Problem: Implement_FarthestFirstTraversal in Ada

## Problem Understanding

The Farthest First Traversal algorithm is a greedy clustering method that starts with one point and repeatedly adds the point farthest from the existing cluster centers.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Implement_FarthestFirstTraversal is
   
   type Point is record
      X : Float;
      Y : Float;
   end record;
   
   type Point_Array is array (Positive range <>) of Point;
   
   -- Calculate Euclidean distance between two points
   function Distance(P1, P2 : Point) return Float is
   begin
      return Sqrt((P1.X - P2.X)**2 + (P1.Y - P2.Y)**2);
   end Distance;
   
   -- Find the point farthest from all existing centers
   function Farthest_Point(Points : Point_Array; Centers : Point_Array) 
      return Positive is
      Max_Distance : Float := 0.0;
      Farthest_Index : Positive := 1;
      
      procedure Update_Max_Distance(Index : Positive) is
         Min_Dist_To_Centers : Float := Float'Last;
      begin
         for C in Centers'Range loop
            declare
               Dist : constant Float := Distance(Points(Index), Centers(C));
            begin
               if Dist < Min_Dist_To_Centers then
                  Min_Dist_To_Centers := Dist;
               end if;
            end;
         end loop;
         
         if Min_Dist_To_Centers > Max_Distance then
            Max_Distance := Min_Dist_To_Centers;
            Farthest_Index := Index;
         end if;
      end Update_Max_Distance;
   begin
      for I in Points'Range loop
         Update_Max_Distance(I);
      end loop;
      
      return Farthest_Index;
   end Farthest_Point;
   
   -- Main algorithm implementation
   procedure Farthest_First_Traversal(Points : Point_Array; K : Positive) is
      Centers : Point_Array(1..K);
      Unvisited : array (Points'Range) of Boolean := (others => True);
      
      -- Find first center (we'll use the first point)
      First_Point : constant Positive := 1;
   begin
      Centers(1) := Points(First_Point);
      Unvisited(First_Point) := False;
      
      -- Add remaining centers
      for I in 2..K loop
         declare
            Next_Index : constant Positive := Farthest_Point(Points, Centers(1..I-1));
         begin
            Centers(I) := Points(Next_Index);
            Unvisited(Next_Index) := False;
         end;
      end loop;
      
      -- Output the centers
      for I in Centers'Range loop
         Put(centers(I).X, Fore => 5, Aft => 3, Exp => 0);
         Put(" ");
         Put(centers(I).Y, Fore => 5, Aft => 3, Exp => 0);
         New_Line;
      end loop;
   end Farthest_First_Traversal;
   
   -- Example usage
   procedure Test_Example is
      Points : Point_Array(1..4) := 
        ((X => 0.0, Y => 0.0),
         (X => 0.0, Y => 1.0),
         (X => 1.0, Y => 0.0),
         (X => 1.0, Y => 1.0));
   begin
      Put_Line("Points:");
      for I in Points'Range loop
         Put(Points(I).X, Fore => 5, Aft => 3, Exp => 0);
         Put(" ");
         Put(Points(I).Y, Fore => 5, Aft => 3, Exp => 0);
         New_Line;
      end loop;
      
      Put_Line("Centers for K=2:");
      Farthest_First_Traversal(Points, 2);
   end Test_Example;
   
begin
   Test_Example;
end Implement_FarthestFirstTraversal;
```

## Algorithm Explanation

1. **Initialization**: Start with the first point as the first center
2. **Greedy Selection**: For each subsequent center, select the point that is farthest from all existing centers
3. **Distance Calculation**: Use Euclidean distance formula: √[(x₂-x₁)² + (y₂-y₁)²]
4. **Farthest Point Logic**: For each unvisited point, calculate the minimum distance to all current centers, then select the point with maximum such minimum distance

## Time Complexity
- O(n²k) where n is the number of points and k is the number of clusters
- Each iteration requires checking all remaining points against all existing centers

## Space Complexity
- O(k + n) for storing centers and unvisited points

This implementation handles the core requirements of the Farthest First Traversal algorithm in Ada, providing a clear separation of concerns with helper functions for distance calculation and point selection.