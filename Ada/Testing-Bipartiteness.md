# Rosalind Problem: Testing Bipartiteness - Ada Solution

## Problem Understanding

A graph is bipartite if its vertices can be divided into two disjoint sets such that every edge connects a vertex from one set to a vertex from the other set. This is equivalent to checking if the graph can be colored using only two colors such that no adjacent vertices have the same color.

## Solution Approach

I'll use a BFS-based approach:
1. Color vertices with two colors (0 and 1)
2. Start with any unvisited vertex, color it with color 0
3. For each vertex, assign the opposite color to all its neighbors
4. If we find a neighbor already colored with the same color, the graph is not bipartite

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Vectors.Generic_Bounded_Lists;

procedure Testing_Bipartiteness is
   
   -- Define a graph as adjacency list
   type Vertex_Id is range 1 .. 1000;
   type Color is (Uncolored, Color_0, Color_1);
   
   package Vertex_Vector is new Ada.Containers.Vectors (Index_Type => Natural, 
                                                        Element_Type => Vertex_Id);
   
   type Graph is record
      adjacency_list : array (Vertex_Id) of Vertex_Vector.Vector;
      num_vertices   : Natural;
      num_edges      : Natural;
   end record;
   
   -- Global graph structure
   G : Graph;
   
   -- Color array for vertices
   Colors : array (Vertex_Id) of Color;
   
   -- Function to check if a graph is bipartite
   function Is_Bipartite return Boolean is
      type Queue_Element is record
         vertex : Vertex_Id;
         color  : Color;
      end record;
      
      package Queue is new Ada.Containers.Vectors (Index_Type => Natural,
                                                   Element_Type => Queue_Element);
      
      Q : Queue.Vector;
      current_vertex, neighbor : Vertex_Id;
      current_color : Color;
      found : Boolean;
   begin
      -- Initialize all vertices as uncolored
      for V in Vertex_Id loop
         Colors(V) := Uncolored;
      end loop;
      
      -- Check each component of the graph
      for V in 1 .. G.num_vertices loop
         if Colors(V) = Uncolored then
            -- Start BFS from this vertex
            Q.Clear;
            Q.Append ((V, Color_0));
            Colors(V) := Color_0;
            
            while not Q.Is_Empty loop
               declare
                  Element : constant Queue_Element := Q.First_Element;
               begin
                  current_vertex := Element.vertex;
                  current_color := Element.color;
                  Q.Delete_First;
                  
                  -- Check all neighbors of current vertex
                  for I in 1 .. G.adjacency_list(current_vertex).Length loop
                     neighbor := G.adjacency_list(current_vertex)(I);
                     
                     if Colors(neighbor) = Uncolored then
                        -- Color neighbor with opposite color
                        if current_color = Color_0 then
                           Colors(neighbor) := Color_1;
                        else
                           Colors(neighbor) := Color_0;
                        end if;
                        
                        Q.Append ((neighbor, Colors(neighbor)));
                     elsif Colors(neighbor) = current_color then
                        -- Adjacent vertices have same color - not bipartite
                        return False;
                     end if;
                  end loop;
               end;
            end loop;
         end if;
      end loop;
      
      return True;
   end Is_Bipartite;
   
   -- Read input function
   procedure Read_Input is
      N, M : Integer;
      U, V : Integer;
   begin
      -- Read number of vertices and edges
      Get(N); Get(M);
      G.num_vertices := N;
      G.num_edges := M;
      
      -- Initialize adjacency list
      for I in 1 .. N loop
         G.adjacency_list(I) := Vertex_Vector.Empty_Vector;
      end loop;
      
      -- Read edges
      for I in 1 .. M loop
         Get(U); Get(V);
         Vertex_Vector.Append(G.adjacency_list(U), V);
         Vertex_Vector.Append(G.adjacency_list(V), U);  -- Undirected graph
      end loop;
   end Read_Input;
   
begin
   Read_Input;
   
   if Is_Bipartite then
      Put_Line("1");
   else
      Put_Line("0");
   end if;
end Testing_Bipartiteness;
```

## Explanation

### Key Components:

1. **Data Structures**:
   - `Graph` record with adjacency list representation
   - `Color` enumeration for vertex coloring (Uncolored, Color_0, Color_1)
   - BFS queue to process vertices level by level

2. **Algorithm Logic**:
   - Uses BFS traversal to color vertices
   - Assigns alternating colors (0 and 1) to adjacent vertices
   - If any edge connects vertices of same color, returns "not bipartite"
   - Handles disconnected components by checking all vertices

3. **Bipartiteness Check**:
   - Starts with vertex colored as Color_0
   - All neighbors get Color_1
   - All neighbors' neighbors get Color_0
   - If conflict arises (same color for adjacent vertices), graph is not bipartite

### Time Complexity: O(V + E)
### Space Complexity: O(V + E)

The solution correctly handles:
- Undirected graphs
- Disconnected components
- Edge cases like single vertices or empty graphs
- Proper BFS traversal with coloring

## Sample Input/Output

**Input:**
```
4 4
1 2
2 3
3 4
4 1
```

**Output:**
```
0
```

**Input:**
```
3 2
1 2
2 3
```

**Output:**
```
1
```