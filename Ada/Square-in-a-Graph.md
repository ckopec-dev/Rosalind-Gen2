# Square in a Graph - Rosalind Problem Solution in Ada

## Problem Understanding

The "Square in a Graph" problem asks us to find a cycle of length 4 (a square) in an undirected graph. A square consists of 4 vertices connected in a cycle, forming a quadrilateral.

## Approach

I'll solve this by:
1. Reading the graph as an adjacency list
2. For each vertex, checking if it can form a 4-cycle with other vertices
3. Using brute force to check all possible combinations of 4 vertices for cycle formation

## Ada Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Square_In_A_Graph is
   
   type Vertex is new Integer range 1 .. 1000;
   type Edge is record
      From, To : Vertex;
   end record;
   
   package Edge_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, 
                                                       Element_Type => Edge);
   
   -- Adjacency list representation
   type Adjacency_List is array (Vertex) of Edge_Vectors.Vector;
   
   -- Graph data structure
   Graph : Adjacency_List;
   Num_Vertices : Integer := 0;
   Num_Edges : Integer := 0;
   
   procedure Read_Input is
      Line : Unbounded_String;
      From, To : Vertex;
   begin
      -- First line contains number of vertices and edges
      Get_Line (Line);
      declare
         S : constant String := To_String (Line);
         Pos : Natural := S'First;
      begin
         while Pos <= S'Last and then S(Pos) = ' ' loop
            Pos := Pos + 1;
         end loop;
         
         From := 0;
         while Pos <= S'Last and then S(Pos) /= ' ' loop
            From := From * 10 + Character'Pos (S(Pos)) - Character'Pos ('0');
            Pos := Pos + 1;
         end loop;
         
         Num_Vertices := From;
         
         To := 0;
         while Pos <= S'Last and then S(Pos) = ' ' loop
            Pos := Pos + 1;
         end loop;
         
         while Pos <= S'Last and then S(Pos) /= ' ' loop
            To := To * 10 + Character'Pos (S(Pos)) - Character'Pos ('0');
            Pos := Pos + 1;
         end loop;
         
         Num_Edges := To;
      end;
      
      -- Read edges
      for I in 1 .. Num_Edges loop
         Get_Line (Line);
         declare
            S : constant String := To_String (Line);
            Pos : Natural := S'First;
            From, To : Vertex := 0;
         begin
            while Pos <= S'Last and then S(Pos) = ' ' loop
               Pos := Pos + 1;
            end loop;
            
            while Pos <= S'Last and then S(Pos) /= ' ' loop
               From := From * 10 + Character'Pos (S(Pos)) - Character'Pos ('0');
               Pos := Pos + 1;
            end loop;
            
            while Pos <= S'Last and then S(Pos) = ' ' loop
               Pos := Pos + 1;
            end loop;
            
            while Pos <= S'Last and then S(Pos) /= ' ' loop
               To := To * 10 + Character'Pos (S(Pos)) - Character'Pos ('0');
               Pos := Pos + 1;
            end loop;
            
            -- Add edge to adjacency list
            Graph(From).Append ((From, To));
            Graph(To).Append ((To, From));
         end;
      end loop;
   end Read_Input;
   
   function Has_Edge (V1, V2 : Vertex) return Boolean is
      -- Check if there's an edge between V1 and V2
   begin
      for I in 1 .. Graph(V1).Length loop
         if Graph(V1)(I).To = V2 then
            return True;
         end if;
      end loop;
      return False;
   end Has_Edge;
   
   function Find_Square return Boolean is
      -- Check all combinations of 4 vertices for a square
      V1, V2, V3, V4 : Vertex;
   begin
      for V1 in 1 .. Num_Vertices loop
         for V2 in (V1 + 1) .. Num_Vertices loop
            -- Check if there's an edge between V1 and V2
            if not Has_Edge (V1, V2) then
               continue;
            end if;
            
            for V3 in (V2 + 1) .. Num_Vertices loop
               -- Check if there's an edge between V2 and V3
               if not Has_Edge (V2, V3) then
                  continue;
               end if;
               
               -- Check if there's an edge between V1 and V3
               if not Has_Edge (V1, V3) then
                  continue;
               end if;
               
               for V4 in (V3 + 1) .. Num_Vertices loop
                  -- Check if there's an edge between V3 and V4
                  if not Has_Edge (V3, V4) then
                     continue;
                  end if;
                  
                  -- Check if there's an edge between V1 and V4
                  if not Has_Edge (V1, V4) then
                     continue;
                  end if;
                  
                  -- Check if there's an edge between V2 and V4
                  if not Has_Edge (V2, V4) then
                     continue;
                  end if;
                  
                  -- Found a square!
                  Put_Line ("4");
                  Put_Line (Integer'Image (V1));
                  Put_Line (Integer'Image (V2));
                  Put_Line (Integer'Image (V3));
                  Put_Line (Integer'Image (V4));
                  return True;
               end loop;
            end loop;
         end loop;
      end loop;
      
      return False;
   end Find_Square;
   
begin
   Read_Input;
   
   if not Find_Square then
      Put_Line ("0");
   end if;
end Square_In_A_Graph;
```

## Explanation

This Ada solution works as follows:

1. **Data Structures**: 
   - Uses an adjacency list representation with a vector of edges for each vertex
   - Defines types for vertices and edges appropriately

2. **Input Reading**:
   - Reads the number of vertices and edges from the first line
   - Processes subsequent lines to build the adjacency list representation

3. **Square Detection**:
   - Uses nested loops to check all combinations of 4 vertices
   - For each combination, verifies that all 4 required edges exist
   - The four edges form a square if: (V1,V2), (V2,V3), (V3,V4), (V1,V4) are present

4. **Output**:
   - If a square is found, outputs "4" followed by the four vertices
   - If no square exists, outputs "0"

## Time Complexity
O(n⁴) where n is the number of vertices in the graph, as we check all combinations of 4 vertices.

## Space Complexity
O(m + n) where m is the number of edges and n is the number of vertices for storing the adjacency list.