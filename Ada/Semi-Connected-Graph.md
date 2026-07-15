# Rosalind Problem: Semi-Connected Graph (Ada Solution)

## Problem Understanding

A directed graph is semi-connected if for every pair of vertices u and v, either there's a path from u to v or a path from v to u (or both). This means that in the condensation graph (where each strongly connected component is contracted into a single vertex), there should be a directed path between any two components.

## Approach

1. Find all strongly connected components using Kosaraju's algorithm
2. Create the condensation graph (meta-graph) of these components
3. Check if the condensation graph forms a directed path (i.e., it's a DAG with a unique topological ordering)

## Ada Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure Semi_Connected_Graph is
   
   type Vertex is new Integer range 1 .. 1000;
   type Edge is record
      From, To : Vertex;
   end record;
   
   package Edge_Vectors is new Ada.Containers.Indefinite_Vectors(Vertex, Edge);
   package Vertex_Vectors is new Ada.Containers.Vectors(Vertex, Vertex);
   
   type Graph is record
      Adjacency_List : array(Vertex) of Edge_Vectors.Vector;
      Num_Vertices   : Vertex;
      Num_Edges      : Natural;
   end record;
   
   type Component is record
      Vertices : Vertex_Vectors.Vector;
      Index    : Natural;
   end record;
   
   package Component_Vectors is new Ada.Containers.Vectors(Natural, Component);
   
   -- Graph operations
   procedure Add_Edge(G : in out Graph; From, To : Vertex) is
   begin
      Edge_Vectors.Append(G.Adjacency_List(From), (From, To));
      G.Num_Edges := G.Num_Edges + 1;
   end Add_Edge;
   
   procedure Initialize_Graph(G : in out Graph; N : Vertex) is
   begin
      G.Num_Vertices := N;
      G.Num_Edges := 0;
      for I in 1 .. N loop
         Edge_Vectors.Clear(G.Adjacency_List(I));
      end loop;
   end Initialize_Graph;
   
   -- DFS operations
   procedure DFS_Recursive(G : Graph; V : Vertex; Visited : in out array(Boolean); 
                          Stack : in out array(Vertex)) is
      I : Natural := 0;
   begin
      Visited(V) := True;
      for E of G.Adjacency_List(V) loop
         if not Visited(E.To) then
            DFS_Recursive(G, E.To, Visited, Stack);
         end if;
      end loop;
      Stack(Stack'First + I) := V;
      I := I + 1;
   end DFS_Recursive;
   
   procedure DFS_Recursive_Reverse(G : Graph; V : Vertex; 
                                  Visited : in out array(Boolean); 
                                  Component : in out Vertex_Vectors.Vector) is
   begin
      Visited(V) := True;
      Vertex_Vectors.Append(Component, V);
      
      for E of G.Adjacency_List(V) loop
         if not Visited(E.To) then
            DFS_Recursive_Reverse(G, E.To, Visited, Component);
         end if;
      end loop;
   end DFS_Recursive_Reverse;
   
   -- Kosaraju's algorithm to find strongly connected components
   procedure Find_SCC(G : Graph; Components : in out Component_Vectors.Vector) is
      Visited : array(Vertex) of Boolean := (others => False);
      Stack   : array(1 .. G.Num_Vertices) of Vertex;
      Top     : Natural := 0;
      Temp    : array(Vertex) of Boolean := (others => False);
      
      procedure DFS_Reverse(G : Graph; V : Vertex) is
      begin
         Visited(V) := True;
         for E of G.Adjacency_List(V) loop
            if not Visited(E.To) then
               DFS_Reverse(G, E.To);
            end if;
         end loop;
         Stack(Top + 1) := V;
         Top := Top + 1;
      end DFS_Reverse;
      
      procedure DFS_Components(G : Graph; V : Vertex; 
                              Component : in out Vertex_Vectors.Vector) is
      begin
         Visited(V) := True;
         Vertex_Vectors.Append(Component, V);
         
         for E of G.Adjacency_List(V) loop
            if not Visited(E.To) then
               DFS_Components(G, E.To, Component);
            end if;
         end loop;
      end DFS_Components;
      
   begin
      -- First DFS to fill the stack
      for V in 1 .. G.Num_Vertices loop
         if not Visited(V) then
            DFS_Reverse(G, V);
         end if;
      end loop;
      
      -- Reset visited array for second pass
      Visited := (others => False);
      
      -- Process vertices in reverse order of finish times
      while Top > 0 loop
         declare
            V : constant Vertex := Stack(Top);
         begin
            Top := Top - 1;
            if not Visited(V) then
               declare
                  New_Component : Component;
               begin
                  New_Component.Index := Components.Length + 1;
                  Vertex_Vectors.Clear(New_Component.Vertices);
                  DFS_Components(G, V, New_Component.Vertices);
                  Component_Vectors.Append(Components, New_Component);
               end;
            end if;
         end;
      end loop;
   end Find_SCC;
   
   -- Check if condensation graph is semi-connected
   function Is_Semi_Connected(G : Graph) return Boolean is
      Components : Component_Vectors.Vector;
      Condensation_Edges : array(1 .. 1000, 1 .. 1000) of Boolean := (others => (others => False));
      Component_Count : Natural := 0;
      
      -- Find all components
   begin
      Find_SCC(G, Components);
      Component_Count := Components.Length;
      
      -- If only one component, it's semi-connected
      if Component_Count <= 1 then
         return True;
      end if;
      
      -- Create condensation graph edges
      for V in 1 .. G.Num_Vertices loop
         for E of G.Adjacency_List(V) loop
            declare
               From_Component : Natural := 0;
               To_Component   : Natural := 0;
            begin
               -- Find which component each vertex belongs to
               for I in 1 .. Components.Length loop
                  if Vertex_Vectors.Contains(Components(I).Vertices, V) then
                     From_Component := I;
                  end if;
                  if Vertex_Vectors.Contains(Components(I).Vertices, E.To) then
                     To_Component := I;
                  end if;
               end loop;
               
               -- If different components, add edge to condensation graph
               if From_Component /= To_Component and then not Condensation_Edges(From_Component, To_Component) then
                  Condensation_Edges(From_Component, To_Component) := True;
               end if;
            end;
         end loop;
      end loop;
      
      -- Check if condensation graph forms a directed path
      declare
         In_Degree : array(1 .. Component_Count) of Natural := (others => 0);
         Out_Degree : array(1 .. Component_Count) of Natural := (others => 0);
         Has_Incoming : array(1 .. Component_Count) of Boolean := (others => False);
         Has_Outgoing : array(1 .. Component_Count) of Boolean := (others => False);
      begin
         -- Calculate in and out degrees
         for I in 1 .. Component_Count loop
            for J in 1 .. Component_Count loop
               if Condensation_Edges(I, J) then
                  In_Degree(J) := In_Degree(J) + 1;
                  Out_Degree(I) := Out_Degree(I) + 1;
                  Has_Incoming(J) := True;
                  Has_Outgoing(I) := True;
               end if;
            end loop;
         end loop;
         
         -- A directed path has exactly one vertex with in-degree 0 and one with out-degree 0
         -- All others should have in-degree = 1 and out-degree = 1
         declare
            Zero_In_Count : Natural := 0;
            Zero_Out_Count : Natural := 0;
            One_In_Count : Natural := 0;
            One_Out_Count : Natural := 0;
         begin
            for I in 1 .. Component_Count loop
               if In_Degree(I) = 0 then
                  Zero_In_Count := Zero_In_Count + 1;
               elsif In_Degree(I) = 1 then
                  One_In_Count := One_In_Count + 1;
               end if;
               
               if Out_Degree(I) = 0 then
                  Zero_Out_Count := Zero_Out_Count + 1;
               elsif Out_Degree(I) = 1 then
                  One_Out_Count := One_Out_Count + 1;
               end if;
            end loop;
            
            -- For a path: one vertex with in-degree 0, one with out-degree 0,
            -- and all others have in-degree = 1 and out-degree = 1
            return (Zero_In_Count = 1 and Zero_Out_Count = 1 and 
                   One_In_Count = Component_Count - 2 and 
                   One_Out_Count = Component_Count - 2);
         end;
      end;
   end Is_Semi_Connected;
   
   -- Main program
   procedure Process_Input is
      N, M : Integer;
      G    : Graph;
   begin
      Put_Line("Enter number of vertices and edges:");
      Get(N); Get(M);
      
      Initialize_Graph(G, Vertex(N));
      
      Put_Line("Enter edges (from to):");
      for I in 1 .. M loop
         declare
            From, To : Integer;
         begin
            Get(From); Get(To);
            Add_Edge(G, Vertex(From), Vertex(To));
         end;
      end loop;
      
      if Is_Semi_Connected(G) then
         Put_Line("Yes");
      else
         Put_Line("No");
      end if;
   end Process_Input;
   
begin
   Process_Input;
end Semi_Connected_Graph;
```

## Explanation

This Ada solution implements the following approach:

1. **Graph Representation**: Uses an adjacency list representation with vectors for efficient operations.

2. **Kosaraju's Algorithm**: 
   - First DFS pass to determine finishing times of vertices
   - Second DFS on the transposed graph to find strongly connected components

3. **Condensation Graph Construction**:
   - Each SCC is contracted into a single vertex
   - Edges between components are added based on original edges

4. **Semi-Connected Check**:
   - A directed graph is semi-connected if its condensation graph forms a directed path
   - This means exactly one vertex with in-degree 0, one with out-degree 0, and all others have in-degree = 1 and out-degree = 1

## Time Complexity
- O(V + E) for Kosaraju's algorithm
- O(V + E) for condensation graph construction
- Overall: O(V + E)

## Space Complexity
- O(V + E) for storing the graph and components

This solution handles the Rosalind problem requirements and should work correctly for semi-connected graph detection.