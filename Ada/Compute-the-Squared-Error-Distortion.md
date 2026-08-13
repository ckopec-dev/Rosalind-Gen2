# Rosalind Problem: Compute the Squared Error Distortion

## Problem Description
Given a set of data points and a set of centers, compute the squared error distortion, which is the sum of squared distances from each point to its nearest center.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Compute_Squared_Error_Distortion is
   
   type Point is array (1 .. 2) of Float;
   type Center is array (1 .. 2) of Float;
   
   -- Function to compute Euclidean distance squared between two points
   function Distance_Squared(p1, p2 : Point) return Float is
      dx : Float := p1(1) - p2(1);
      dy : Float := p1(2) - p2(2);
   begin
      return dx * dx + dy * dy;
   end Distance_Squared;
   
   -- Function to find the index of the nearest center to a point
   function Nearest_Center(point : Point; centers : array of Center) return Positive is
      min_distance : Float := Distance_Squared(point, centers(1));
      nearest_index : Positive := 1;
   begin
      for i in 2 .. centers'Length loop
         declare
            distance : Float := Distance_Squared(point, centers(i));
         begin
            if distance < min_distance then
               min_distance := distance;
               nearest_index := i;
            end if;
         end;
      end loop;
      return nearest_index;
   end Nearest_Center;
   
   -- Main computation function
   function Compute_Distortion(points : array of Point; centers : array of Center) return Float is
      total_error : Float := 0.0;
   begin
      for i in points'Range loop
         declare
            nearest_idx : Positive := Nearest_Center(points(i), centers);
            distance_sq : Float := Distance_Squared(points(i), centers(nearest_idx));
         begin
            total_error := total_error + distance_sq;
         end;
      end loop;
      return total_error / Float(points'Length);
   end Compute_Distortion;
   
   -- Example data (replace with actual input)
   Points : array (1 .. 5) of Point :=
     ((1.0, 2.0), (3.0, 4.0), (5.0, 6.0), (7.0, 8.0), (9.0, 10.0));
   
   Centers : array (1 .. 2) of Center :=
     ((2.0, 3.0), (8.0, 9.0));
   
   distortion : Float;
   
begin
   distortion := Compute_Distortion(Points, Centers);
   Put("Squared Error Distortion: ");
   Put(distortion, Fore => 1, Aft => 6, Exp => 0);
   New_Line;
end Compute_Squared_Error_Distortion;
```

## Input Format
The input consists of:
- A set of data points in 2D space (each point has x and y coordinates)
- A set of centers (cluster centers)

## Output
The squared error distortion value, which is the average of squared distances from each point to its nearest center.

## Algorithm Steps
1. For each data point, find the nearest center
2. Calculate the squared Euclidean distance from the point to its nearest center
3. Sum all these squared distances
4. Divide by the number of points to get the average (distortion)

## Time Complexity
O(n × k) where n is the number of points and k is the number of centers

## Space Complexity
O(1) additional space (excluding input storage)

Note: This solution assumes 2D points and includes example data. For a complete Rosalind solution, you would need to read input from files or standard input according to the specific problem requirements.