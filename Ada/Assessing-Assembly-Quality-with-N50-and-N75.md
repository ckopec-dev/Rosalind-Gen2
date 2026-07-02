# Rosalind Problem: Assessing Assembly Quality with N50 and N75

## Problem Understanding

The N50 and N75 statistics are used to assess the quality of genome assemblies. Given a list of contig lengths, we need to:
- Calculate N50: the smallest contig length such that at least 50% of all bases are in contigs of this length or longer
- Calculate N75: the smallest contig length such that at least 75% of all bases are in contigs of this length or longer

## Solution Approach

1. Read input contig lengths
2. Sort lengths in descending order
3. Calculate total assembly size
4. Find cumulative sum and determine N50 and N75 thresholds
5. Output the results

## Ada Implementation

```ada
with Ada.Text_IO; 
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Containers.Vectors;
with Ada.Directories;

procedure Assess_Assembly_Quality is
   package Int_Vect is new Ada.Containers.Vectors (Positive, Integer);
   use Int_Vect;
   
   -- Read contig lengths from input
   procedure Read_Contigs (Contigs : in out Vector) is
      Line : String(1..1000);
      Last : Natural;
      Value : Integer;
   begin
      loop
         exit when not Ada.Text_IO.Standard_Input.Is_Open;
         begin
            Ada.Text_IO.Get_Line(Line, Last);
            if Last > 0 then
               Value := Integer'Value(Line(1..Last));
               Append(Contigs, Value);
            end if;
         exception
            when others =>
               null; -- Skip invalid lines
         end;
      end loop;
   end Read_Contigs;
   
   -- Sort contigs in descending order
   procedure Sort_Descending (Contigs : in out Vector) is
      Temp : Integer;
      N : constant Natural := Length(Contigs);
   begin
      for I in 1..N-1 loop
         for J in I+1..N loop
            if Element(Contigs, I) < Element(Contigs, J) then
               Temp := Element(Contigs, I);
               Replace_Element(Contigs, I, Element(Contigs, J));
               Replace_Element(Contigs, J, Temp);
            end if;
         end loop;
      end loop;
   end Sort_Descending;
   
   -- Calculate N50 and N75
   procedure Calculate_N50_N75 (Contigs : Vector; N50 : out Integer; N75 : out Integer) is
      Total_Size : Long_Long_Integer := 0;
      Cumulative_Size : Long_Long_Integer := 0;
      Threshold_50 : Long_Long_Integer;
      Threshold_75 : Long_Long_Integer;
      Found_50 : Boolean := False;
      Found_75 : Boolean := False;
   begin
      -- Calculate total size
      for I in 1..Length(Contigs) loop
         Total_Size := Total_Size + Long_Long_Integer(Element(Contigs, I));
      end loop;
      
      Threshold_50 := Total_Size / 2;
      Threshold_75 := (3 * Total_Size) / 4;
      
      -- Find N50 and N75
      for I in 1..Length(Contigs) loop
         Cumulative_Size := Cumulative_Size + Long_Long_Integer(Element(Contigs, I));
         
         if not Found_50 and Cumulative_Size >= Threshold_50 then
            N50 := Element(Contigs, I);
            Found_50 := True;
         end if;
         
         if not Found_75 and Cumulative_Size >= Threshold_75 then
            N75 := Element(Contigs, I);
            Found_75 := True;
         end if;
         
         exit when Found_50 and Found_75;
      end loop;
   end Calculate_N50_N75;
   
   Contigs : Vector;
   N50 : Integer;
   N75 : Integer;
begin
   -- Read contig lengths
   Read_Contigs(Contigs);
   
   -- Sort in descending order
   Sort_Descending(Contigs);
   
   -- Calculate N50 and N75
   Calculate_N50_N75(Contigs, N50, N75);
   
   -- Output results
   Ada.Text_IO.Put_Line(Integer'Image(N50));
   Ada.Text_IO.Put_Line(Integer'Image(N75));
end Assess_Assembly_Quality;
```

## Sample Input/Output

**Input:**
```
100
200
300
400
500
```

**Output:**
```
400
300
```

## Explanation

For the example:
- Total assembly size = 1500
- N50 threshold = 750 bases
- Cumulative sum progression:
  - 500 (cumulative: 500) < 750
  - 400 (cumulative: 900) ≥ 750 → N50 = 400
- N75 threshold = 1125 bases
- Cumulative sum progression:
  - 500 (cumulative: 500) < 1125
  - 400 (cumulative: 900) < 1125
  - 300 (cumulative: 1200) ≥ 1125 → N75 = 300

## Key Features of the Solution

1. **Vector-based storage** for dynamic contig length handling
2. **Proper sorting** in descending order to prioritize larger contigs
3. **Efficient calculation** using cumulative sums
4. **Robust error handling** for invalid input
5. **Memory efficient** approach without storing unnecessary intermediate data

The solution correctly implements the N50 and N75 metrics which are standard measures for genome assembly quality assessment.