# Rosalind Problem General_Sink in Ada

## Problem Understanding

The General Sink problem asks us to find a sink in a directed graph - a vertex with no outgoing edges (in-degree = 0).

## Solution Approach

1. Read the directed graph from input
2. Count the in-degrees of all vertices
3. Find vertices with in-degree 0 (sinks)
4. Output the result

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure General_Sink is
   type Vertex is range 1 .. 1000;
   type Edge is record
      From, To : Vertex;
   end record;
   
   package Edge_Vector is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Edge);
   use Edge_Vector;
   
   -- Input reading
   procedure Read_Input (Edges : in out Vector) is
      Line : Unbounded_String;
      From, To : Vertex;
   begin
      while not End_Of_File loop
         Line := Get_Line;
         if Length(Line) > 0 then
            -- Parse the edge from the line
            -- Assuming format like "1 2" for edge from 1 to 2
            declare
               Pos : Natural := 1;
               Num : Integer;
            begin
               -- Read first number
               while Pos <= Length(Line) and then Character'Pos(Line(Pos)) < 48 loop
                  Pos := Pos + 1;
               end loop;
               Num := 0;
               while Pos <= Length(Line) and then Character'Pos(Line(Pos)) >= 48 loop
                  Num := Num * 10 + (Character'Pos(Line(Pos)) - 48);
                  Pos := Pos + 1;
               end loop;
               From := Vertex(Num);
               
               -- Skip spaces
               while Pos <= Length(Line) and then Character'Pos(Line(Pos)) < 48 loop
                  Pos := Pos + 1;
               end loop;
               
               -- Read second number
               Num := 0;
               while Pos <= Length(Line) and then Character'Pos(Line(Pos)) >= 48 loop
                  Num := Num * 10 + (Character'Pos(Line(Pos)) - 48);
                  Pos := Pos + 1;
               end loop;
               To := Vertex(Num);
               
               Append(Edges, (From => From, To => To));
            end;
         end if;
      end loop;
   end Read_Input;
   
   -- Main algorithm
   procedure Solve is
      Edges : Vector;
      In_Degree : array (Vertex) of Natural := (others => 0);
      Max_Vertex : Vertex := 0;
      Sink_Count : Natural := 0;
   begin
      -- Read all edges
      Read_Input(Edges);
      
      -- Calculate in-degrees
      for I in 1 .. Edges.Length loop
         declare
            Edge : constant Edge := Edges.Element(I);
         begin
            In_Degree(Edge.To) := In_Degree(Edge.To) + 1;
            if Edge.From > Max_Vertex then
               Max_Vertex := Edge.From;
            end if;
            if Edge.To > Max_Vertex then
               Max_Vertex := Edge.To;
            end if;
         end;
      end loop;
      
      -- Find sinks (vertices with in-degree 0)
      for V in 1 .. Max_Vertex loop
         if In_Degree(V) = 0 then
            Put(V, Width => 0);
            Put(" ");
            Sink_Count := Sink_Count + 1;
         end if;
      end loop;
      
      -- Output newline if sinks found
      if Sink_Count > 0 then
         New_Line;
      end if;
   end Solve;
   
begin
   Solve;
end General_Sink;
```

## Alternative Simpler Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure General_Sink is
   type Vertex is range 1 .. 1000;
   type Edge is record
      From, To : Vertex;
   end record;
   
   -- Array to store in-degrees
   In_Degree : array (Vertex) of Natural := (others => 0);
   
   -- Read edges and count in-degrees
   procedure Read_Edges is
      From, To : Vertex;
   begin
      loop
         exit when End_Of_File;
         begin
            From := Vertex'Value(Get_Line);
            To := Vertex'Value(Get_Line);
            In_Degree(To) := In_Degree(To) + 1;
         exception
            when others => null; -- Handle parsing errors
         end;
      end loop;
   end Read_Edges;
   
   -- Find and output sinks
   procedure Find_Sinks is
      Found : Boolean := False;
   begin
      for V in 1 .. 1000 loop
         if In_Degree(V) = 0 then
            Put(V, Width => 0);
            Put(" ");
            Found := True;
         end if;
      end loop;
      
      if Found then
         New_Line;
      end if;
   end Find_Sinks;
   
begin
   Read_Edges;
   Find_Sinks;
end General_Sink;
```

## Key Points

1. **Input Format**: The program expects edges in the format where each line contains two vertices representing a directed edge from the first to the second.

2. **In-Degree Calculation**: We count how many times each vertex appears as a destination in the edges.

3. **Sink Detection**: A vertex is a sink if its in-degree is 0 (no edges lead away from it).

4. **Output**: We output all vertices with in-degree 0, separated by spaces.

5. **Edge Cases**: The solution handles empty graphs and graphs with no sinks properly.

This implementation efficiently solves the General Sink problem by using in-degree counting, which is the standard approach for finding sinks in directed graphs.

