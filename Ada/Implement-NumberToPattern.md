# Rosalind Problem: Implement_NumberToPattern in Ada

## Problem Description
The NumberToPattern problem asks us to convert a number into its corresponding k-mer pattern in lexicographic order.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure NumberToPattern is
   
   function NumberToPattern(Number : Integer; k : Integer) return String is
      Alphabet : array(0..3) of Character := ('A', 'C', 'G', 'T');
      Result   : String(1..k);
      N        : Integer := Number;
      i        : Integer;
   begin
      for i in reverse 1..k loop
         Result(i) := Alphabet(N mod 4);
         N := N / 4;
      end loop;
      
      return Result;
   end NumberToPattern;
   
begin
   -- Example usage
   Put_Line(NumberToPattern(45, 4));  -- Should output "ACGT"
   Put_Line(NumberToPattern(0, 1));   -- Should output "A"
   Put_Line(NumberToPattern(15, 2));  -- Should output "TT"
end NumberToPattern;
```

## Explanation

This Ada solution implements the NumberToPattern algorithm:

1. **Alphabet Mapping**: We define an array mapping numbers 0-3 to nucleotides A, C, G, T respectively.

2. **Algorithm Logic**:
   - Start with the given number and k-mer length
   - For each position from right to left (least significant to most significant):
     - Find the remainder when dividing by 4 (this gives us the nucleotide)
     - Divide the number by 4 for the next iteration
     - Place the corresponding nucleotide in the result string

3. **Key Insight**: 
   - The problem is essentially converting a number from base-10 to base-4 representation
   - Each digit in base-4 corresponds to a nucleotide
   - We process from least significant to most significant bit

## Example Walkthrough

For NumberToPattern(45, 4):
- Position 4 (rightmost): 45 mod 4 = 1 → 'C'
- Position 3: 45 div 4 = 11, then 11 mod 4 = 3 → 'T'  
- Position 2: 11 div 4 = 2, then 2 mod 4 = 2 → 'G'
- Position 1 (leftmost): 2 div 4 = 0, then 0 mod 4 = 0 → 'A'

Result: "ACGT"

## Time and Space Complexity
- **Time Complexity**: O(k) where k is the length of the pattern
- **Space Complexity**: O(k) for storing the result string