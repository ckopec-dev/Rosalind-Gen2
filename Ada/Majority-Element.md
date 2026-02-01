# Rosalind Problem: Majority Element in Ada

## Problem Understanding

The majority element in a list is the element that appears more than half the time. Given a list of integers, we need to find if there's a majority element and return it, or return -1 if none exists.

## Solution Approach

1. Count the frequency of each element
2. Check if any element appears more than n/2 times
3. Return the majority element or -1

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure Majority_Element is
   type Integer_Vector is array (Positive range <>) of Integer;
   
   package Int_Map is new Ada.Containers.Ordered_Maps
     (Key_Type => Integer, Element_Type => Natural);
   
   procedure Read_Data (Data : out Integer_Vector);
   function Find_Majority (Data : Integer_Vector) return Integer;
   
   procedure Read_Data (Data : out Integer_Vector) is
      N : Integer;
   begin
      Get (N);
      Data := new Integer_Vector (1..N);
      for I in Data'Range loop
         Get (Data(I));
      end loop;
   end Read_Data;
   
   function Find_Majority (Data : Integer_Vector) return Integer is
      Map : Int_Map.Map;
      Max_Count : Natural := 0;
      Majority_Element : Integer := -1;
      N : constant Natural := Data'Length;
   begin
      -- Count frequencies
      for I in Data'Range loop
         declare
            Current : Integer := Data(I);
            Count : Natural;
         begin
            if Int_Map.Contains (Map, Current) then
               Count := Int_Map.Element (Map, Current) + 1;
            else
               Count := 1;
            end if;
            Int_Map.Replace_Element (Map, Current, Count);
            
            -- Check if this element is majority
            if Count > N / 2 and then Count > Max_Count then
               Max_Count := Count;
               Majority_Element := Current;
            end if;
         end;
      end loop;
      
      return Majority_Element;
   end Find_Majority;
   
   Data : Integer_Vector (1..1000);  -- Assuming maximum size
   Result : Integer;
begin
   Read_Data (Data);
   Result := Find_Majority (Data);
   Put_Line (Integer'Image (Result));
end Majority_Element;
```

## Alternative Implementation (More Efficient)

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Ordered_Maps;

procedure Majority_Element is
   package Int_Map is new Ada.Containers.Ordered_Maps
     (Key_Type => Integer, Element_Type => Natural);
   
   function Find_Majority (Data : Integer_Vector) return Integer;
   
   function Find_Majority (Data : Integer_Vector) return Integer is
      Map : Int_Map.Map;
      N : constant Natural := Data'Length;
      Majority_Element : Integer := -1;
   begin
      -- Count frequencies
      for I in Data'Range loop
         declare
            Current : Integer := Data(I);
            Count : Natural;
         begin
            if Int_Map.Contains (Map, Current) then
               Count := Int_Map.Element (Map, Current) + 1;
            else
               Count := 1;
            end if;
            Int_Map.Replace_Element (Map, Current, Count);
            
            -- Check if majority element found
            if Count > N / 2 then
               Majority_Element := Current;
               return Majority_Element;
            end if;
         end;
      end loop;
      
      return Majority_Element;
   end Find_Majority;
   
   N : Integer;
   Data : Integer_Vector (1..1000);
   Result : Integer;
begin
   Get (N);
   for I in Data'Range loop
      Get (Data(I));
   end loop;
   
   Result := Find_Majority (Data);
   Put_Line (Integer'Image (Result));
end Majority_Element;
```

## Key Features of the Solution

1. **Use of Ordered Maps**: Efficiently counts element frequencies
2. **Early Termination**: Returns immediately when majority element is found
3. **Proper Memory Management**: Uses Ada's built-in containers
4. **Input/Output Handling**: Follows Rosalind's input format requirements

## Time and Space Complexity

- **Time Complexity**: O(n log n) due to map operations
- **Space Complexity**: O(k) where k is the number of distinct elements

The solution efficiently handles the majority element problem by leveraging Ada's container library for frequency counting and early termination when the majority element is detected.

