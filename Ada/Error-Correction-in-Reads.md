# Rosalind Problem: Error Correction in Reads (Ada Solution)

## Problem Understanding

The task is to identify and correct sequencing errors in DNA reads by finding:
1. Reads that appear exactly once (potential errors)
2. Reads that are reverse complements of other reads
3. Reads that differ by one nucleotide from another read (correction candidates)

## Solution Approach

I'll implement a solution using:
- String manipulation for DNA sequences
- Hash maps to count occurrences
- Reverse complement computation
- Hamming distance calculation

## Ada Implementation

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Numerics.Discrete_Random;

procedure Error_Correction_In_Reads is
   
   type DNA_Read is array(1..100) of Character;
   
   -- Hash map to store read counts
   package Read_Map is new Ada.Containers.Hashed_Maps
     (Key_Type => Unbounded_String,
      Element_Type => Natural,
      Hash => String_Hash,
      Equivalent_Keys => "=");
   
   type Read_Counts is array(1..100) of Natural;
   
   -- Function to compute reverse complement
   function Reverse_Complement(Read : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String := Null_Unbounded_String;
      Ch : Character;
   begin
      for I in reverse 1..Length(Read) loop
         Ch := Element(Read, I);
         case Ch is
            when 'A' => Append(Result, 'T');
            when 'T' => Append(Result, 'A');
            when 'G' => Append(Result, 'C');
            when 'C' => Append(Result, 'G');
            when others => Append(Result, Ch);
         end case;
      end loop;
      return Result;
   end Reverse_Complement;
   
   -- Function to calculate Hamming distance between two strings
   function Hamming_Distance(S1, S2 : Unbounded_String) return Natural is
      Distance : Natural := 0;
   begin
      if Length(S1) /= Length(S2) then
         return -1;  -- Invalid: different lengths
      end if;
      
      for I in 1..Length(S1) loop
         if Element(S1, I) /= Element(S2, I) then
            Distance := Distance + 1;
         end if;
      end loop;
      
      return Distance;
   end Hamming_Distance;
   
   -- Function to check if two strings are reverse complements
   function Are_Reverse_Complements(S1, S2 : Unbounded_String) return Boolean is
   begin
      return Reverse_Complement(S1) = S2;
   end Are_Reverse_Complements;
   
   -- Main processing function
   procedure Process_Reads(Reads : in out Read_Map.Map) is
      Valid_Reads : Read_Map.Map;
      Invalid_Reads : Read_Map.Map;
      Corrected_Pairs : array(1..100, 1..2) of Unbounded_String;
      Pair_Count : Natural := 0;
      
      -- Process all reads
   begin
      -- Separate valid and invalid reads
      for Element : constant Read_Map.Cursor of Reads loop
         declare
            Read : constant Unbounded_String := Read_Map.Key(Element);
            Count : constant Natural := Read_Map.Element(Element);
         begin
            if Count = 1 then
               Invalid_Reads.Insert(Read, 1);
            else
               Valid_Reads.Insert(Read, Count);
            end if;
         end;
      end loop;
      
      -- Find corrections for invalid reads
      for Invalid_Element : constant Read_Map.Cursor of Invalid_Reads loop
         declare
            Invalid_Read : constant Unbounded_String := Read_Map.Key(Invalid_Element);
         begin
            -- Check if it's a reverse complement of a valid read
            for Valid_Element : constant Read_Map.Cursor of Valid_Reads loop
               declare
                  Valid_Read : constant Unbounded_String := Read_Map.Key(Valid_Element);
               begin
                  if Are_Reverse_Complements(Invalid_Read, Valid_Read) then
                     Pair_Count := Pair_Count + 1;
                     Corrected_Pairs(Pair_Count, 1) := Invalid_Read;
                     Corrected_Pairs(Pair_Count, 2) := Valid_Read;
                     exit;
                  end if;
               end;
            end loop;
            
            -- If not reverse complement, check if it differs by one base from valid read
            if Pair_Count = 0 or else 
              (Pair_Count > 0 and then Corrected_Pairs(Pair_Count, 1) /= Invalid_Read) then
               for Valid_Element : constant Read_Map.Cursor of Valid_Reads loop
                  declare
                     Valid_Read : constant Unbounded_String := Read_Map.Key(Valid_Element);
                  begin
                     if Hamming_Distance(Invalid_Read, Valid_Read) = 1 then
                        Pair_Count := Pair_Count + 1;
                        Corrected_Pairs(Pair_Count, 1) := Invalid_Read;
                        Corrected_Pairs(Pair_Count, 2) := Valid_Read;
                        exit;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
      
      -- Output results
      for I in 1..Pair_Count loop
         Put_Line(To_String(Corrected_Pairs(I, 1)) & " -> " & To_String(Corrected_Pairs(I, 2)));
      end loop;
   end Process_Reads;
   
begin
   -- Example input processing (would be read from file in real problem)
   declare
      Reads : Read_Map.Map;
      Input_Reads : array(1..5) of Unbounded_String := 
        (To_Unbounded_String("CATCG"),
         To_Unbounded_String("AAATTC"),
         To_Unbounded_String("TTCGAA"),
         To_Unbounded_String("CATCG"),
         To_Unbounded_String("ATTCGA"));
   begin
      -- Count occurrences
      for I in Input_Reads'Range loop
         declare
            Read : constant Unbounded_String := Input_Reads(I);
         begin
            if Reads.Contains(Read) then
               Reads.Replace_Element(Read, Reads.Element(Read) + 1);
            else
               Reads.Insert(Read, 1);
            end if;
         end;
      end loop;
      
      -- Process reads for error correction
      Process_Reads(Reads);
   end;
   
end Error_Correction_In_Reads;
```

## Key Features of the Solution

1. **Reverse Complement Function**: Computes reverse complement using standard DNA base pairing rules (A↔T, G↔C)

2. **Hamming Distance Calculation**: Computes the number of differences between two strings of equal length

3. **Error Detection Logic**:
   - Identifies reads appearing exactly once
   - Checks if invalid reads are reverse complements of valid ones
   - Finds reads differing by one nucleotide from valid reads

4. **Data Structures**:
   - Uses hash maps for efficient read counting
   - Stores corrections in a 2D array for output formatting

## How to Use

1. Compile the Ada program using an Ada compiler (like GNAT)
2. Modify the `Input_Reads` array with actual DNA reads from your data file
3. Run the program to see error correction results

## Expected Output Format

The program outputs corrected pairs in the format:
```
INVALID_READ -> VALID_READ
```

Where each pair represents an error-corrected read and its corresponding valid reference sequence.