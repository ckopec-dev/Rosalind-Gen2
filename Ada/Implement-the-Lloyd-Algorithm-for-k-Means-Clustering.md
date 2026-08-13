# Rosalind Problem: Implement the Lloyd Algorithm for k-Means Clustering

## Problem Description
Implement the Lloyd algorithm for k-means clustering, which is a classic iterative algorithm that partitions data points into k clusters by minimizing the sum of squared distances from each point to its assigned cluster centroid.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Lloyd_Algorithm is
   
   type Point is record
      X : Float;
      Y : Float;
   end record;
   
   type Cluster_Centroid is record
      X : Float;
      Y : Float;
   end record;
   
   type Point_Array is array (Positive range <>) of Point;
   type Centroid_Array is array (Positive range <>) of Cluster_Centroid;
   
   -- Calculate Euclidean distance between two points
   function Distance(P1, P2 : Point) return Float is
   begin
      return Sqrt((P1.X - P2.X)**2.0 + (P1.Y - P2.Y)**2.0);
   end Distance;
   
   -- Find the closest centroid to a point
   function Closest_Centroid(Point : Point; Centroids : Centroid_Array) 
      return Positive is
      Min_Dist : Float := Float'Last;
      Closest_Index : Positive := 1;
   begin
      for I in Centroids'Range loop
         declare
            Dist : constant Float := Distance(Point, Centroids(I));
         begin
            if Dist < Min_Dist then
               Min_Dist := Dist;
               Closest_Index := I;
            end if;
         end;
      end loop;
      return Closest_Index;
   end Closest_Centroid;
   
   -- Calculate new centroids based on current cluster assignments
   function Update_Centroids(Points : Point_Array; 
                            Assignments : array of Positive; 
                            K : Positive) return Centroid_Array is
      Centroids : Centroid_Array(1..K);
      Count : array (1..K) of Natural := (others => 0);
   begin
      -- Initialize centroids to zero
      for I in Centroids'Range loop
         Centroids(I).X := 0.0;
         Centroids(I).Y := 0.0;
      end loop;
      
      -- Sum coordinates for each cluster
      for I in Points'Range loop
         declare
            Cluster_ID : constant Positive := Assignments(I);
         begin
            Centroids(Cluster_ID).X := Centroids(Cluster_ID).X + Points(I).X;
            Centroids(Cluster_ID).Y := Centroids(Cluster_ID).Y + Points(I).Y;
            Count(Cluster_ID) := Count(Cluster_ID) + 1;
         end;
      end loop;
      
      -- Calculate average (mean) for each cluster
      for I in Centroids'Range loop
         if Count(I) > 0 then
            Centroids(I).X := Centroids(I).X / Float(Count(I));
            Centroids(I).Y := Centroids(I).Y / Float(Count(I));
         end if;
      end loop;
      
      return Centroids;
   end Update_Centroids;
   
   -- Lloyd Algorithm Implementation
   procedure Lloyd_Algorithm(Points : Point_Array; 
                            K : Positive;
                            Max_Iterations : Positive := 100) is
      Current_Centroids : Centroid_Array(1..K);
      Previous_Centroids : Centroid_Array(1..K);
      Assignments : array (Points'Range) of Positive;
      Converged : Boolean := False;
      Iteration : Natural := 0;
   begin
      -- Initialize centroids randomly
      for I in Current_Centroids'Range loop
         Current_Centroids(I).X := Float(Random * 10.0);
         Current_Centroids(I).Y := Float(Random * 10.0);
      end loop;
      
      while not Converged and Iteration < Max_Iterations loop
         -- Store previous centroids for convergence check
         Previous_Centroids := Current_Centroids;
         
         -- Assign points to closest centroid
         for I in Points'Range loop
            Assignments(I) := Closest_Centroid(Points(I), Current_Centroids);
         end loop;
         
         -- Update centroids
         Current_Centroids := Update_Centroids(Points, Assignments, K);
         
         -- Check for convergence (if centroids haven't changed significantly)
         Converged := True;
         for I in Current_Centroids'Range loop
            if abs(Current_Centroids(I).X - Previous_Centroids(I).X) > 0.001 or
               abs(Current_Centroids(I).Y - Previous_Centroids(I).Y) > 0.001 then
               Converged := False;
               exit;
            end if;
         end loop;
         
         Iteration := Iteration + 1;
      end loop;
      
      -- Output final centroids
      for I in Current_Centroids'Range loop
         Put("Centroid ");
         Put(I, Fore => 1, Aft => 0, Exp => 0);
         Put(" : (");
         Put(Current_Centroids(I).X, Fore => 1, Aft => 3, Exp => 0);
         Put(", ");
         Put(Current_Centroids(I).Y, Fore => 1, Aft => 3, Exp => 0);
         Put_Line(")");
      end loop;
   end Lloyd_Algorithm;
   
   -- Sample data points
   Points : constant Point_Array(1..6) := 
     ((X => 1.0, Y => 2.0),
      (X => 4.0, Y => 6.0),
      (X => 8.0, Y => 9.0),
      (X => 3.0, Y => 5.0),
      (X => 7.0, Y => 8.0),
      (X => 2.0, Y => 3.0));
   
begin
   Put_Line("Lloyd Algorithm for k-Means Clustering");
   Put_Line("=======================================");
   Put_Line("Data points:");
   for I in Points'Range loop
      Put("Point ");
      Put(I, Fore => 1, Aft => 0, Exp => 0);
      Put(" : (");
      Put(Points(I).X, Fore => 1, Aft => 1, Exp => 0);
      Put(", ");
      Put(Points(I).Y, Fore => 1, Aft => 1, Exp => 0);
      Put_Line(")");
   end loop;
   
   Put_Line("");
   Lloyd_Algorithm(Points, K => 2);
end Lloyd_Algorithm;
```

## Explanation

This Ada implementation of the Lloyd algorithm for k-means clustering includes:

1. **Data Types**: 
   - `Point` record to represent 2D coordinates
   - `Cluster_Centroid` record for cluster centers
   - Arrays to store points and centroids

2. **Key Functions**:
   - `Distance`: Calculates Euclidean distance between two points
   - `Closest_Centroid`: Finds the nearest centroid for a given point
   - `Update_Centroids`: Recalculates centroids based on current cluster assignments
   - `Lloyd_Algorithm`: Main algorithm implementation

3. **Algorithm Steps**:
   - Initialize k centroids randomly
   - Assign each point to the closest centroid
   - Update centroids as the mean of assigned points
   - Repeat until convergence or maximum iterations reached

4. **Convergence Check**: The algorithm stops when centroids don't change significantly between iterations.

## Input/Output Format

The program takes a set of 2D points and k (number of clusters) as input, then outputs the final cluster centroids after the Lloyd algorithm has converged.

## Example Usage

For the sample data with 6 points and k=2, the algorithm will partition the points into two clusters and output the coordinates of the final centroids for each cluster.