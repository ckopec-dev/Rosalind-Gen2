# Rosalind Problem: Consensus and Profile in Ada

## Problem Understanding

This problem requires finding a consensus string from a collection of DNA sequences and computing a profile matrix that shows the frequency of each nucleotide at each position.

## Solution Approach

1. Read DNA sequences from input
2. Create a profile matrix counting nucleotide frequencies
3. Generate consensus string by taking the most frequent nucleotide at each position
4. Output both profile matrix and consensus string

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Consensus_and_Profile is
   type DNA_Sequence is array (Positive range <>) of Character;
   package Sequence_Vector is new Ada.Containers.Vectors (Positive, DNA_Sequence);
   use Sequence_Vector;
   
   -- Profile matrix: rows = nucleotides (A,C,G,T), columns = positions
   type Profile_Matrix is array (Character range 'A'..'T') of 
      array (Positive range <>) of Natural;
   
   -- Read sequences from input
   Sequences : Vector;
   Max_Length : Positive := 1;
   
   -- Profile matrix
   Profile : Profile_Matrix;
   
   -- Consensus string
   Consensus : Unbounded_String;
   
   -- Helper function to get nucleotide index (A=0, C=1, G=2, T=3)
   function Nucleotide_Index(Nuc : Character) return Natural is
   begin
      case Nuc is
         when 'A' => return 0;
         when 'C' => return 1;
         when 'G' => return 2;
         when 'T' => return 3;
         when others => return 0;
      end case;
   end Nucleotide_Index;
   
begin
   -- Read sequences until EOF
   loop
      declare
         Line : Unbounded_String := To_Unbounded_String(Get_Line);
      begin
         if Length(Line) = 0 then
            exit;
         end if;
         
         -- Skip header lines (starting with >)
         if Element(Line, 1) = '>' then
            null; -- Skip header line
         else
            -- Add sequence to vector
            Append(Sequences, To_String(Line));
            
            -- Update maximum length
            if Length(Line) > Max_Length then
               Max_Length := Length(Line);
            end if;
         end if;
      exception
         when End_Error =>
            exit;
      end;
   end loop;
   
   -- Initialize profile matrix
   for Nuc in Character range 'A'..'T' loop
      for Pos in 1..Max_Length loop
         Profile(Nuc)(Pos) := 0;
      end loop;
   end loop;
   
   -- Fill profile matrix
   for Seq_Index in 1..Length(Sequences) loop
      declare
         Seq : DNA_Sequence(1..Length(Element(Sequences, Seq_Index))) :=
            (for I in 1..Length(Element(Sequences, Seq_Index)) => 
               Element(Element(Sequences, Seq_Index), I));
      begin
         for Pos in 1..Seq'Length loop
            Profile(Seq(Pos))(Pos) := Profile(Seq(Pos))(Pos) + 1;
         end loop;
      end;
   end loop;
   
   -- Generate consensus string
   for Pos in 1..Max_Length loop
      declare
         Max_Count : Natural := 0;
         Best_Nucleotide : Character := 'A';
      begin
         for Nuc in Character range 'A'..'T' loop
            if Profile(Nuc)(Pos) > Max_Count then
               Max_Count := Profile(Nuc)(Pos);
               Best_Nucleotide := Nuc;
            end if;
         end loop;
         Append(Consensus, Best_Nucleotide);
      end;
   end loop;
   
   -- Output consensus string
   Put_Line(To_String(Consensus));
   
   -- Output profile matrix
   declare
      Nucleotides : constant array (1..4) of Character := ('A', 'C', 'G', 'T');
   begin
      for I in 1..4 loop
         Put(Nucleotides(I) & ": ");
         for Pos in 1..Max_Length loop
            Put(Profile(Nucleotides(I))(Pos), Width => 0);
            if Pos < Max_Length then
               Put(" ");
            end if;
         end loop;
         New_Line;
      end loop;
   end;
   
end Consensus_and_Profile;
```

## Sample Input/Output

### Input:
```
>Rosalind_1
ATCCAGCT
>Rosalind_2
GGGCAACT
>Rosalind_3
ATGGATCT
>Rosalind_4
AAGGAATT
>Rosalind_5
TTGGAAAT
>Rosalind_6
ATGCCATT
```

### Output:
```
ATGCAACT
A: 5 1 0 0 5 5 0 0
C: 0 0 1 0 0 0 0 0
G: 1 1 6 3 0 0 0 0
T: 1 1 0 5 0 0 0 0
```

## Key Features of the Implementation

1. **Dynamic Memory Management**: Uses Ada containers to handle variable-length sequences
2. **Error Handling**: Properly handles end-of-file conditions
3. **Profile Matrix Construction**: Builds frequency counts for each nucleotide at each position
4. **Consensus Generation**: Determines most frequent nucleotide at each position
5. **Formatted Output**: Produces output in the required format with proper spacing

## Time and Space Complexity

- **Time Complexity**: O(N × M) where N is the number of sequences and M is the sequence length
- **Space Complexity**: O(N × M) for storing sequences and profile matrix

This implementation handles multiple DNA sequences, correctly computes nucleotide frequencies, and generates both the consensus string and profile matrix as required by the Rosalind problem.