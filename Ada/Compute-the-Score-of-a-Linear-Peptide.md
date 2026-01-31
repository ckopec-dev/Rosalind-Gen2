# Rosalind Problem: Compute the Score of a Linear Peptide

## Problem Description
Given a linear peptide and a scoring matrix, compute the score of the peptide.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Compute_The_Score_Of_A_Linear_Peptide is
   
   -- Define amino acid masses
   type Amino_Acid is (A, R, N, D, C, Q, E, G, H, I, L, K, M, F, P, S, T, W, Y, V);
   type Mass_Array is array (Amino_Acid) of Integer;
   
   -- Predefined mass table
   Mass_Table : constant Mass_Array := (
      A => 71,  R => 156, N => 114, D => 115, C => 103,
      Q => 128, E => 129, G => 57,  H => 137, I => 113,
      L => 113, K => 128, M => 131, F => 147, P => 97,
      S => 87,  T => 101, W => 186, Y => 163, V => 99
   );
   
   -- Function to get mass of an amino acid
   function Get_Mass(AA : Amino_Acid) return Integer is
   begin
      return Mass_Table(AA);
   end Get_Mass;
   
   -- Function to convert character to amino acid
   function Char_To_AA(C : Character) return Amino_Acid is
   begin
      case C is
         when 'A' => return A;
         when 'R' => return R;
         when 'N' => return N;
         when 'D' => return D;
         when 'C' => return C;
         when 'Q' => return Q;
         when 'E' => return E;
         when 'G' => return G;
         when 'H' => return H;
         when 'I' => return I;
         when 'L' => return L;
         when 'K' => return K;
         when 'M' => return M;
         when 'F' => return F;
         when 'P' => return P;
         when 'S' => return S;
         when 'T' => return T;
         when 'W' => return W;
         when 'Y' => return Y;
         when 'V' => return V;
         when others => raise Constraint_Error;
      end case;
   end Char_To_AA;
   
   -- Function to compute the score of a linear peptide
   function Compute_Score(Peptide : String) return Integer is
      Score : Integer := 0;
      AA : Amino_Acid;
   begin
      for I in Peptide'First .. Peptide'Last loop
         AA := Char_To_AA(Peptide(I));
         Score := Score + Get_Mass(AA);
      end loop;
      return Score;
   end Compute_Score;
   
   -- Main program
   procedure Main is
      Peptide : String(1..10) := "NQV";
      Score : Integer;
   begin
      Score := Compute_Score(Peptide);
      Put("Peptide: ");
      Put(Peptide);
      Put_Line("");
      Put("Score: ");
      Put(Score);
      Put_Line("");
   end Main;
   
begin
   Main;
end Compute_The_Score_Of_A_Linear_Peptide;
```

## Explanation

This Ada program solves the Rosalind problem "Compute the Score of a Linear Peptide" by:

1. **Defining amino acid types**: Creates an enumeration of all 20 amino acids with their standard single-letter codes.

2. **Mass table**: Maintains a lookup table with the exact masses for each amino acid as specified in the problem.

3. **Helper functions**:
   - `Get_Mass`: Returns the mass of a given amino acid
   - `Char_To_AA`: Converts a character to its corresponding amino acid type
   - `Compute_Score`: Calculates the total score by summing up the masses of all amino acids in the peptide

4. **Main logic**: The `Compute_Score` function iterates through each character in the peptide string, converts it to an amino acid, looks up its mass, and accumulates the total score.

## Sample Input/Output

For input peptide "NQV":
- N = 114
- Q = 128  
- V = 99
- Total Score = 341

The program can be easily modified to read from standard input or file by changing the peptide string and adding input handling code.

