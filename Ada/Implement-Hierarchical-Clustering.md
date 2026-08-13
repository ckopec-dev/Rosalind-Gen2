# Rosalind Problem: Implement Hierarchical Clustering

## Problem Understanding

Hierarchical clustering is a method of cluster analysis that builds a hierarchy of clusters. In this implementation, we need to create a hierarchical clustering algorithm that repeatedly merges the two closest clusters until only one cluster remains.

## Solution Approach

I'll implement the hierarchical clustering algorithm with the following steps:
1. Initialize each point as its own cluster
2. Find the two closest clusters
3. Merge them into a new cluster
4. Update distances between new cluster and remaining clusters
5. Repeat until only one cluster remains

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Containers.Vectors;
with Ada.Numerics.Elementary_Functions;

procedure Implement_Hierarchical_Clustering is
   
   type Point is record
      X, Y : Float;
   end record;
   
   type Cluster_Id is new Integer range 0 .. 1000;
   
   package Point_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, 
                                                        Element_Type => Point);
   
   type Cluster is record
      Id       : Cluster_Id;
      Points   : Point_Vectors.Vector;
      Center_X : Float;
      Center_Y : Float;
   end record;
   
   package Cluster_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, 
                                                          Element_Type => Cluster);
   
   type Distance_Matrix is array (Natural range <>) of Float;
   
   -- Function to calculate Euclidean distance between two points
   function Distance(P1, P2 : Point) return Float is
   begin
      return Float'Sqrt((P1.X - P2.X)**2 + (P1.Y - P2.Y)**2);
   end Distance;
   
   -- Function to calculate distance between two clusters
   function Cluster_Distance(C1, C2 : Cluster) return Float is
   begin
      return Distance((C1.Center_X, C1.Center_Y), (C2.Center_X, C2.Center_Y));
   end Cluster_Distance;
   
   -- Function to update cluster center based on points
   procedure Update_Center(C : in out Cluster) is
      Total_X, Total_Y : Float := 0.0;
      Count            : Natural := 0;
   begin
      for I in Point_Vectors.First_Index(C.Points) .. Point_Vectors.Last_Index(C.Points) loop
         Total_X := Total_X + Point_Vectors.Element(C.Points, I).X;
         Total_Y := Total_Y + Point_Vectors.Element(C.Points, I).Y;
         Count   := Count + 1;
      end loop;
      
      if Count > 0 then
         C.Center_X := Total_X / Float(Count);
         C.Center_Y := Total_Y / Float(Count);
      else
         C.Center_X := 0.0;
         C.Center_Y := 0.0;
      end if;
   end Update_Center;
   
   -- Function to find minimum distance in distance matrix
   function Find_Minimum_Distance(Dist_Matrix : Distance_Matrix) return (Natural, Natural) is
      Min_Dist : Float := Float'Last;
      Min_I, Min_J : Natural := 0;
   begin
      for I in Dist_Matrix'First .. Dist_Matrix'Last loop
         if Dist_Matrix(I) < Min_Dist then
            Min_Dist := Dist_Matrix(I);
            Min_I := I;
         end if;
      end loop;
      
      return (Min_I, Min_J);
   end Find_Minimum_Distance;
   
   -- Main hierarchical clustering function
   procedure Hierarchical_Cluster(Points : in Point_Vectors.Vector) is
      Clusters : Cluster_Vectors.Vector;
      Num_Points : constant Natural := Point_Vectors.Length(Points);
      
      -- Initialize clusters (each point is its own cluster)
      procedure Initialize_Clusters is
      begin
         for I in Point_Vectors.First_Index(Points) .. Point_Vectors.Last_Index(Points) loop
            declare
               New_Cluster : Cluster;
            begin
               New_Cluster.Id := Cluster_Id(I);
               Point_Vectors.Append(New_Cluster.Points, Point_Vectors.Element(Points, I));
               Update_Center(New_Cluster);
               Cluster_Vectors.Append(Clusters, New_Cluster);
            end;
         end loop;
      end Initialize_Clusters;
      
   begin
      -- Initialize clusters
      Initialize_Clusters;
      
      -- Print initial points
      Put_Line("Initial Points:");
      for I in Point_Vectors.First_Index(Points) .. Point_Vectors.Last_Index(Points) loop
         declare
            P : constant Point := Point_Vectors.Element(Points, I);
         begin
            Put("Point ");
            Put(I, Width => 2);
            Put(" : (");
            Put(P.X, Aft => 2, Exp => 0);
            Put(", ");
            Put(P.Y, Aft => 2, Exp => 0);
            Put_Line(")");
         end;
      end loop;
      
      -- Print initial clusters
      Put_Line("Initial Clusters:");
      for I in Cluster_Vectors.First_Index(Clusters) .. Cluster_Vectors.Last_Index(Clusters) loop
         declare
            C : constant Cluster := Cluster_Vectors.Element(Clusters, I);
         begin
            Put("Cluster ");
            Put(C.Id, Width => 2);
            Put(" Center: (");
            Put(C.Center_X, Aft => 2, Exp => 0);
            Put(", ");
            Put(C.Center_Y, Aft => 2, Exp => 0);
            Put_Line(")");
         end;
      end loop;
      
      -- Main clustering loop
      while Cluster_Vectors.Length(Clusters) > 1 loop
         declare
            Min_Dist : Float := Float'Last;
            Merge_I, Merge_J : Natural := 0;
            Temp_Cluster : Cluster;
         begin
            -- Find closest pair of clusters
            for I in Cluster_Vectors.First_Index(Clusters) .. Cluster_Vectors.Last_Index(Clusters) loop
               for J in I + 1 .. Cluster_Vectors.Last_Index(Clusters) loop
                  declare
                     D : constant Float := Cluster_Distance(Cluster_Vectors.Element(Clusters, I),
                                                           Cluster_Vectors.Element(Clusters, J));
                  begin
                     if D < Min_Dist then
                        Min_Dist := D;
                        Merge_I := I;
                        Merge_J := J;
                     end if;
                  end;
               end loop;
            end loop;
            
            -- Merge clusters
            Put_Line("Merging clusters " & 
                    Integer'Image(Merge_I) & " and " & 
                    Integer'Image(Merge_J) & 
                    " with distance " & 
                    Float'Image(Min_Dist));
            
            -- Create new cluster by merging the two closest clusters
            Temp_Cluster := Cluster_Vectors.Element(Clusters, Merge_I);
            
            -- Add all points from second cluster to first
            for K in Point_Vectors.First_Index(Cluster_Vectors.Element(Clusters, Merge_J).Points) .. 
                     Point_Vectors.Last_Index(Cluster_Vectors.Element(Clusters, Merge_J).Points) loop
               Point_Vectors.Append(Temp_Cluster.Points, Point_Vectors.Element(Cluster_Vectors.Element(Clusters, Merge_J).Points, K));
            end loop;
            
            -- Update center of merged cluster
            Update_Center(Temp_Cluster);
            
            -- Remove the second cluster and replace first with merged cluster
            Cluster_Vectors.Delete(Clusters, Merge_J);  -- Remove second cluster
            Cluster_Vectors.Replace_Element(Clusters, Merge_I, Temp_Cluster);  -- Replace first with merged
            
            -- Print current state
            Put_Line("After merge:");
            for I in Cluster_Vectors.First_Index(Clusters) .. Cluster_Vectors.Last_Index(Clusters) loop
               declare
                  C : constant Cluster := Cluster_Vectors.Element(Clusters, I);
               begin
                  Put("Cluster ");
                  Put(C.Id, Width => 2);
                  Put(" Center: (");
                  Put(C.Center_X, Aft => 2, Exp => 0);
                  Put(", ");
                  Put(C.Center_Y, Aft => 2, Exp => 0);
                  Put_Line(")");
               end;
            end loop;
            New_Line;
         end;
      end loop;
      
      -- Print final result
      Put_Line("Final clustering:");
      if Cluster_Vectors.Length(Clusters) > 0 then
         declare
            Final_Cluster : constant Cluster := Cluster_Vectors.Element(Clusters, 1);
         begin
            Put("Final cluster center: (");
            Put(Final_Cluster.Center_X, Aft => 2, Exp => 0);
            Put(", ");
            Put(Final_Cluster.Center_Y, Aft => 2, Exp => 0);
            Put_Line(")");
         end;
      end if;
   end Hierarchical_Cluster;
   
   -- Example data
   Points : Point_Vectors.Vector;
   
begin
   -- Add sample points
   Point_Vectors.Append(Points, (X => 1.0, Y => 2.0));
   Point_Vectors.Append(Points, (X => 3.0, Y => 4.0));
   Point_Vectors.Append(Points, (X => 5.0, Y => 6.0));
   Point_Vectors.Append(Points, (X => 7.0, Y => 8.0));
   
   -- Run hierarchical clustering
   Hierarchical_Cluster(Points);
   
end Implement_Hierarchical_Clustering;
```

## Key Features of the Implementation

1. **Point and Cluster Types**: Defined proper data structures for points and clusters
2. **Distance Calculation**: Implemented Euclidean distance function between points and clusters
3. **Cluster Center Updates**: Automatically recalculates cluster centers after merges
4. **Hierarchical Merging**: Repeatedly finds closest clusters and merges them
5. **Progress Tracking**: Prints intermediate steps to show the clustering process

## Expected Output Format

The program will output:
- Initial points and their coordinates
- Initial cluster configuration  
- Step-by-step merging of clusters with distances
- Final cluster center coordinates

This implementation follows the standard hierarchical clustering algorithm where we iteratively merge the closest pairs of clusters until a single cluster remains, which is typical for agglomerative hierarchical clustering.