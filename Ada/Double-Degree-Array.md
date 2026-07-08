# Rosalind Problem: Double-Degree Array in Ada

## Problem Understanding

The Double-Degree Array problem asks us to compute the "double degree" of each vertex in a graph. The double degree of a vertex is the sum of degrees of all its adjacent vertices.

## Solution Approach

1. Read the graph edges
2. Build an adjacency list representation
3. Calculate the degree of each vertex
4. For each vertex, sum up the degrees of its neighbors to get the double degree

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure Double_Degree_Array is
   
   type Vertex is new Integer range 1..10000;
   type Edge is array (1..2) of Vertex;
   
   package Vertex_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Vertex);
   
   package Vertex_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Vertex, Element_Type => Vertex_Vectors.Vector);
   
   -- Read input data
   procedure Read_Input (N : out Integer; M : out Integer; Edges : out Edge_Array) is
      Line : String(1..100);
      Last : Natural;
   begin
      Get_Line(Line, Last);
      N := 0; M := 0;
      for I in 1..Last loop
         if Line(I) = ' ' then
            M := Integer'Value(Line(1..I-1));
            N := Integer'Value(Line(I+1..Last));
            exit;
         end if;
      end loop;
      
      -- Read edges
      for I in 1..M loop
         Get_Line(Line, Last);
         declare
            Pos : Natural := 0;
         begin
            for J in 1..Last loop
               if Line(J) = ' ' then
                  Pos := J;
                  exit;
               end if;
            end loop;
            Edges(I) := (Vertex'Value(Line(1..Pos-1)), 
                        Vertex'Value(Line(Pos+1..Last)));
         end;
      end loop;
   end Read_Input;
   
   -- Build adjacency list
   procedure Build_Adjacency_List (Edges : Edge_Array; M : Integer; 
                                  Adjacency : out Vertex_Maps.Map) is
      procedure Add_Edge (V1, V2 : Vertex) is
      begin
         if not Adjacency.Contains(V1) then
            Adjacency.Insert(V1, Vertex_Vectors.Empty_Vector);
         end if;
         if not Adjacency.Contains(V2) then
            Adjacency.Insert(V2, Vertex_Vectors.Empty_Vector);
         end if;
         Adjacency.Element(V1).Append(V2);
         Adjacency.Element(V2).Append(V1);
      end Add_Edge;
   begin
      for I in 1..M loop
         Add_Edge(Edges(I)(1), Edges(I)(2));
      end loop;
   end Build_Adjacency_List;
   
   -- Calculate degrees of vertices
   procedure Calculate_Degrees (N : Integer; Adjacency : Vertex_Maps.Map; 
                               Degrees : out array of Integer) is
   begin
      for I in 1..N loop
         if Adjacency.Contains(Vertex(I)) then
            Degrees(I) := Integer(Adjacency.Element(Vertex(I)).Length);
         else
            Degrees(I) := 0;
         end if;
      end loop;
   end Calculate_Degrees;
   
   -- Calculate double degrees
   procedure Calculate_Double_Degrees (N : Integer; Adjacency : Vertex_Maps.Map; 
                                      Degrees : array of Integer; 
                                      Double_Degrees : out array of Integer) is
   begin
      for I in 1..N loop
         Double_Degrees(I) := 0;
         if Adjacency.Contains(Vertex(I)) then
            declare
               Neighbors : Vertex_Vectors.Vector renames Adjacency.Element(Vertex(I));
            begin
               for J in 1..Neighbors.Length loop
                  Double_Degrees(I) := Double_Degrees(I) + Degrees(Integer(Neighbors.Element(J)));
               end loop;
            end;
         end if;
      end loop;
   end Calculate_Double_Degrees;
   
   -- Main execution
   N, M : Integer;
   Edges : array (1..10000) of Edge;
   Adjacency : Vertex_Maps.Map;
   Degrees : array (1..10000) of Integer;
   Double_Degrees : array (1..10000) of Integer;
   
begin
   -- Read input
   Read_Input(N, M, Edges);
   
   -- Build adjacency list
   Build_Adjacency_List(Edges, M, Adjacency);
   
   -- Calculate degrees
   Calculate_Degrees(N, Adjacency, Degrees);
   
   -- Calculate double degrees
   Calculate_Double_Degrees(N, Adjacency, Degrees, Double_Degrees);
   
   -- Output results
   for I in 1..N loop
      Put(Double_Degrees(I)); Put(" ");
   end loop;
   New_Line;
   
end Double_Degree_Array;
```

## Key Features of the Solution

1. **Data Structures**: 
   - Uses vectors for adjacency lists
   - Uses ordered maps for efficient vertex lookups
   - Handles up to 10,000 vertices as per problem constraints

2. **Algorithm Steps**:
   - Read input graph edges
   - Build adjacency list representation
   - Calculate degree of each vertex
   - Compute double degree by summing neighbor degrees

3. **Error Handling**: 
   - Uses appropriate bounds checking
   - Handles edge cases like isolated vertices

## Time Complexity
- O(M + N) where M is the number of edges and N is the number of vertices
- Efficient linear time solution

## Space Complexity
- O(M + N) for storing adjacency list and degree information

This implementation correctly solves the double-degree array problem by efficiently building the graph representation and computing the required values.