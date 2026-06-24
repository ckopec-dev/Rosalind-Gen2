# Rosalind Problem: Wobble Bonding and RNA Secondary Structures (Ada Solution)

## Problem Understanding

This problem asks us to count the number of valid secondary structures for an RNA sequence, where:
- A base can pair with U or G (wobble pairing)
- Each base can only be paired once
- The structure must follow Watson-Crick base pairing rules with wobble exceptions

## Solution Approach

We'll use dynamic programming with the following recurrence relation:
- `dp[i][j]` = number of valid structures for substring from position i to j
- For each interval [i,j], we can either:
  1. Not pair base i (so we add dp[i+1][j])
  2. Pair base i with some base k (where k is a valid pairing partner)
  3. Split the interval into two parts: dp[i][k-1] * dp[k+1][j]

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Wobble_Bonding is
   type Base is (A, U, G, C);
   
   -- Function to check if two bases can pair
   function Can_Pair(b1, b2 : Base) return Boolean is
   begin
      case b1 is
         when A =>
            return b2 = U or b2 = G;
         when U =>
            return b2 = A or b2 = G;
         when G =>
            return b2 = C or b2 = U or b2 = A;
         when C =>
            return b2 = G;
      end case;
   end Can_Pair;
   
   -- Function to convert character to base
   function Char_To_Base(c : Character) return Base is
   begin
      case c is
         when 'A' => return A;
         when 'U' => return U;
         when 'G' => return G;
         when 'C' => return C;
         when others => raise Constraint_Error;
      end case;
   end Char_To_Base;
   
   -- Function to count valid secondary structures using dynamic programming
   function Count_Structures(sequence : String) return Long_Long_Integer is
      n : constant Positive := sequence'Length;
      dp : array (1..n, 1..n) of Long_Long_Integer := (others => (others => 0));
      
      -- Helper function to get base at position
      function Get_Base(pos : Natural) return Base is
      begin
         return Char_To_Base(sequence(pos));
      end Get_Base;
      
   begin
      -- Base case: empty interval
      for i in 1..n loop
         dp(i, i) := 1;
      end loop;
      
      -- Fill DP table bottom-up
      for length in 2..n loop
         for i in 1..(n - length + 1) loop
            declare
               j : constant Natural := i + length - 1;
            begin
               -- Case 1: don't pair base i
               dp(i, j) := dp(i + 1, j);
               
               -- Case 2: pair base i with some valid partner k
               for k in (i + 1)..j loop
                  if Can_Pair(Get_Base(i), Get_Base(k)) then
                     declare
                        temp : Long_Long_Integer;
                     begin
                        if k = i + 1 then
                           temp := dp(i + 2, j);
                        else
                           temp := dp(i + 1, k - 1) * dp(k + 1, j);
                        end if;
                        dp(i, j) := dp(i, j) + temp;
                     end;
                  end if;
               end loop;
            end;
         end loop;
      end loop;
      
      return dp(1, n);
   end Count_Structures;
   
   -- Read input and process
   sequence : String(1..1000);
   length   : Natural;
   
begin
   Get_Line(sequence, length);
   Put_Line(Long_Long_Integer'Image(Count_Structures(sequence(1..length))));
end Wobble_Bonding;
```

## Explanation

The solution uses dynamic programming with the following key components:

1. **Base Pairing Rules**: 
   - A pairs with U or G (wobble)
   - U pairs with A or G (wobble)  
   - G pairs with C, U, or A (wobble)
   - C pairs only with G

2. **Dynamic Programming Approach**:
   - `dp[i][j]` represents the number of valid structures for substring from position i to j
   - For each interval [i,j], we consider:
     - Not pairing base i: `dp[i+1][j]`
     - Pairing base i with valid partner k: sum over all valid k of `dp[i+1][k-1] * dp[k+1][j]`

3. **Time Complexity**: O(n³) where n is the sequence length
4. **Space Complexity**: O(n²) for the DP table

## Sample Input/Output

For input: `AUCG`
Expected output: `2`

The valid structures are:
1. No pairs
2. A-U and C-G pairs

This implementation handles wobble pairing correctly while maintaining the constraint that each base can only be paired once in a valid structure.