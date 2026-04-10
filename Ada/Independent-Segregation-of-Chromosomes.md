# Rosalind Problem: Independent Segregation of Chromosomes

## Problem Understanding

The problem asks us to calculate the probability that at least one of n independent events occurs, where each event has probability p of occurring.

This is equivalent to finding the probability that at least one of n independent trials succeeds, where each trial succeeds with probability p.

## Mathematical Approach

For n independent events, each with probability p of occurring:
- Probability that all events fail = (1-p)^n
- Probability that at least one event occurs = 1 - (1-p)^n

## Ada Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Independent_Segregation_of_Chromosomes is
   -- Function to calculate probability that at least one event occurs
   function Probability_At_Least_One(n: Integer; p: Float) return Float is
      -- Probability that all events fail = (1-p)^n
      -- Probability that at least one event occurs = 1 - (1-p)^n
   begin
      return 1.0 - (1.0 - p) ** Float(n);
   end Probability_At_Least_One;
   
   -- Read input values
   n: Integer;
   p: Float;
   
begin
   -- Read n (number of independent events)
   Get(n);
   
   -- Read p (probability of each event occurring)
   Get(p);
   
   -- Calculate and output the result
   Put(Probability_At_Least_One(n, p), Fore => 1, Aft => 6, Exp => 0);
   New_Line;
   
end Independent_Segregation_of_Chromosomes;
```

## Example Usage

If we have the input:
```
3
0.5
```

The calculation would be:
- Probability that all 3 events fail = (1-0.5)^3 = 0.5^3 = 0.125
- Probability that at least one event occurs = 1 - 0.125 = 0.875

## Key Features of the Solution

1. **Input Handling**: Reads n (number of events) and p (probability) from standard input
2. **Mathematical Calculation**: Uses the complement rule to calculate the probability
3. **Precision**: Uses Float type for decimal precision
4. **Output Formatting**: Formats the output to 6 decimal places
5. **Efficiency**: Direct calculation without unnecessary loops or recursion

## Alternative Implementation with Error Checking

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Independent_Segregation_of_Chromosomes is
   -- Function to calculate probability that at least one event occurs
   function Probability_At_Least_One(n: Integer; p: Float) return Float is
   begin
      -- Validate inputs
      if n < 0 then
         raise Constraint_Error with "Number of events must be non-negative";
      end if;
      
      if p < 0.0 or p > 1.0 then
         raise Constraint_Error with "Probability must be between 0 and 1";
      end if;
      
      return 1.0 - (1.0 - p) ** Float(n);
   end Probability_At_Least_One;
   
   n: Integer;
   p: Float;
   
begin
   -- Read n (number of independent events)
   Get(n);
   
   -- Read p (probability of each event occurring)
   Get(p);
   
   -- Calculate and output the result
   Put(Probability_At_Least_One(n, p), Fore => 1, Aft => 6, Exp => 0);
   New_Line;
   
exception
   when Constraint_Error =>
      Put_Line("Error: Invalid input values");
end Independent_Segregation_of_Chromosomes;
```

This solution efficiently computes the probability using the mathematical principle that the probability of at least one event occurring is 1 minus the probability that all events fail.

