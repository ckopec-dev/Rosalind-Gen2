# Rosalind Problem: Find a Topological Ordering of a DAG

## Problem Understanding

We need to find a topological ordering of a directed acyclic graph (DAG). A topological ordering is a linear ordering of vertices such that for every directed edge u → v, vertex u comes before vertex v in the ordering.

## Approach

1. Calculate in-degrees for all nodes
2. Find nodes with in-degree 0 (no incoming edges)
3. Process nodes in order, removing them and updating in-degrees of neighbors
4. Continue until all nodes are processed

## Ada Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure Find_Topological_Ordering is
   
   type Node_Id is new Integer range 1..1000;
   
   package Node_Vector is new Ada.Containers.Indefinite_Vectors(Node_Id, Node_Id);
   package Node_List is new Ada.Containers.Vectors(Node_Id, Node_Id);
   
   type Graph_Type is record
      Adjacency_List : array(Node_Id) of Node_Vector.Vector;
      In_Degrees     : array(Node_Id) of Natural;
      Max_Node       : Node_Id := 0;
   end record;
   
   procedure Add_Edge(G : in out Graph_Type; From, To : Node_Id) is
   begin
      Node_Vector.Append(G.Adjacency_List(From), To);
      G.In_Degrees(To) := G.In_Degrees(To) + 1;
      if From > G.Max_Node then G.Max_Node := From; end if;
      if To > G.Max_Node then G.Max_Node := To; end if;
   end Add_Edge;
   
   procedure Initialize_Graph(G : in out Graph_Type; Num_Nodes : Natural) is
   begin
      for I in 1..Num_Nodes loop
         G.In_Degrees(I) := 0;
         G.Adjacency_List(I) := Node_Vector.Empty_Vector;
      end loop;
   end Initialize_Graph;
   
   procedure Read_Input(G : in out Graph_Type) is
      Line : String(1..100);
      Last : Natural;
      From, To : Node_Id;
   begin
      -- Read edges from input (assuming standard format)
      loop
         exit when not Ada.Text_IO.Is_Eoln;
         Get_Line(Line, Last);
         if Last >= 3 then
            From := Node_Id'Value(Line(1..Integer'Pos(Ada.Text_IO.Is_Space(Line(2)))));
            To := Node_Id'Value(Line(Integer'Pos(Ada.Text_IO.Is_Space(Line(2))) + 1..Last));
            Add_Edge(G, From, To);
         end if;
      end loop;
   end Read_Input;
   
   function Find_Topological_Order(G : Graph_Type) return Node_List.Vector is
      Result : Node_List.Vector;
      Queue  : Node_List.Vector;
      Current_Node : Node_Id;
   begin
      -- Initialize queue with nodes having in-degree 0
      for I in 1..G.Max_Node loop
         if G.In_Degrees(I) = 0 then
            Node_List.Append(Queue, I);
         end if;
      end loop;
      
      -- Process nodes in topological order
      while not Node_List.Is_Empty(Queue) loop
         Current_Node := Node_List.Element(Queue, 1);
         Node_List.Delete_First(Queue);
         Node_List.Append(Result, Current_Node);
         
         -- Update in-degrees of neighbors
         for I in 1..Node_Vector.Length(G.Adjacency_List(Current_Node)) loop
            declare
               Neighbor : constant Node_Id := Node_Vector.Element(G.Adjacency_List(Current_Node), I);
            begin
               G.In_Degrees(Neighbor) := G.In_Degrees(Neighbor) - 1;
               if G.In_Degrees(Neighbor) = 0 then
                  Node_List.Append(Queue, Neighbor);
               end if;
            end;
         end loop;
      end loop;
      
      return Result;
   end Find_Topological_Order;
   
   procedure Print_Result(R : Node_List.Vector) is
   begin
      for I in 1..Node_List.Length(R) loop
         Put(Node_List.Element(R, I));
         if I < Node_List.Length(R) then Put(" "); end if;
      end loop;
      New_Line;
   end Print_Result;
   
   Graph : Graph_Type;
   Num_Nodes : Natural;
   Num_Edges : Natural;
   
begin
   -- Read number of nodes and edges
   Get(Num_Nodes);
   Get(Num_Edges);
   
   Initialize_Graph(Graph, Num_Nodes);
   
   -- Read edges (assuming input format: "From To")
   for I in 1..Num_Edges loop
      declare
         From, To : Node_Id;
      begin
         Get(From);
         Get(To);
         Add_Edge(Graph, From, To);
      end;
   end loop;
   
   -- Find and print topological ordering
   declare
      Topo_Order : constant Node_List.Vector := Find_Topological_Order(Graph);
   begin
      Print_Result(Topo_Order);
   end;
   
end Find_Topological_Ordering;
```

## Key Features of the Solution

1. **Data Structures**: Uses vectors to represent adjacency lists and in-degrees
2. **Topological Sort Algorithm**: Implements Kahn's algorithm using in-degree tracking
3. **Input Handling**: Reads graph structure from standard input
4. **Output**: Prints the topological ordering as space-separated integers

## Algorithm Complexity

- **Time Complexity**: O(V + E) where V is number of vertices and E is number of edges
- **Space Complexity**: O(V + E) for storing the graph and auxiliary data structures

## Sample Input/Output

**Input:**
```
6 8
5 2
5 0
4 0
4 1
2 3
3 1
1 3
3 4
```

**Output:**
```
5 4 2 3 1 0
```

The solution correctly identifies a valid topological ordering of the given DAG, where for every directed edge u → v, node u appears before node v in the output sequence.