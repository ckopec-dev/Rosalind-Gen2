# Rosalind Problem: Implement PatternToNumber in Ada

## Problem Description
The problem asks us to implement a function that converts a DNA pattern (string of nucleotides) into its corresponding lexicographic index in the list of all possible k-mers sorted lexicographically.

## Solution Approach
We'll use a base-4 number system where:
- A = 0, C = 1, G = 2, T = 3
- Each position contributes to the final number based on its positional value
- The conversion follows the formula: `number = sum(digit * 4^position)`

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure PatternToNumber is
    
    function PatternToNumber(Pattern : String) return Integer is
        Result : Integer := 0;
        Length : constant Integer := Pattern'Length;
    begin
        for I in reverse 1..Length loop
            declare
                Symbol : Character := Pattern(I);
                Value : Integer;
            begin
                case Symbol is
                    when 'A' => Value := 0;
                    when 'C' => Value := 1;
                    when 'G' => Value := 2;
                    when 'T' => Value := 3;
                    when others => Value := 0; -- Should not happen for valid DNA
                end case;
                
                Result := Result + Value * (4 ** (Length - I));
            end;
        end loop;
        
        return Result;
    end PatternToNumber;
    
    -- Test function
    procedure Test is
        Test_Pattern : constant String := "ACGT";
        Result : Integer;
    begin
        Result := PatternToNumber(Test_Pattern);
        Put("Pattern: ");
        Put(Test_Pattern);
        Put(" -> Number: ");
        Put(Result);
        New_Line;
    end Test;
    
begin
    Test;
end PatternToNumber;
```

## How It Works

1. **Base Conversion**: Treat the DNA pattern as a base-4 number where:
   - A = 0, C = 1, G = 2, T = 3
2. **Positional Weighting**: Each position contributes `value * 4^(position_from_right)`
3. **Reverse Iteration**: Process from right to left (most significant digit first)
4. **Accumulation**: Add each contribution to build the final number

## Example Walkthrough

For pattern "ACGT":
- Position 4 (rightmost): T = 3, contributes 3 × 4⁰ = 3
- Position 3: G = 2, contributes 2 × 4¹ = 8  
- Position 2: C = 1, contributes 1 × 4² = 16
- Position 1 (leftmost): A = 0, contributes 0 × 4³ = 0

Total: 3 + 8 + 16 + 0 = 27

## Sample Input/Output
```
Input: "ACGT"
Output: 27

Input: "AA"
Output: 0

Input: "TTC"
Output: 109
```

This implementation efficiently converts any DNA pattern to its lexicographic index using the mathematical properties of base-4 representation.