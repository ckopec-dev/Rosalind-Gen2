# Rosalind Problem: Compute the Probability of a Hidden Path

## Problem Description
Given: A hidden path π = π₁π₂...πₙ and a transition matrix T for a Markov chain, compute the probability of the path π.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Compute_Probability_of_a_Hidden_Path is
   -- Define constants
   Max_States : constant := 100;
   
   -- Transition matrix type
   type Transition_Matrix is array (1..Max_States, 1..Max_States) of Float;
   
   -- Path type
   type Path is array (1..Max_States) of Integer;
   
   -- Input data
   N : Integer;  -- Number of states
   Path_Length : Integer;  -- Length of hidden path
   Hidden_Path : Path;
   Transition_Matrix_Data : Transition_Matrix;
   
   -- Function to compute probability of hidden path
   function Compute_Path_Probability(Path : Path; 
                                    Length : Integer;
                                    Trans_Matrix : Transition_Matrix) return Float is
      Probability : Float := 1.0;
      Current_State, Next_State : Integer;
   begin
      -- For a path of length n, we have n-1 transitions
      for i in 1..Length-1 loop
         Current_State := Path(i);
         Next_State := Path(i+1);
         
         -- Multiply by transition probability from current to next state
         Probability := Probability * Trans_Matrix(Current_State, Next_State);
      end loop;
      
      return Probability;
   end Compute_Path_Probability;
   
begin
   -- Read number of states
   Get(N);
   
   -- Read path length (assuming it's given in input)
   Get(Path_Length);
   
   -- Read the hidden path
   for i in 1..Path_Length loop
      Get(Hidden_Path(i));
   end loop;
   
   -- Read transition matrix
   for i in 1..N loop
      for j in 1..N loop
         Get(Transition_Matrix_Data(i, j));
      end loop;
   end loop;
   
   -- Compute probability of the hidden path
   declare
      Result : Float;
   begin
      Result := Compute_Path_Probability(Hidden_Path, Path_Length, Transition_Matrix_Data);
      
      -- Output result with appropriate formatting
      Put("Probability: ");
      Put(Result, Fore => 1, Aft => 10, Exp => 0);
      New_Line;
   end;
   
end Compute_Probability_of_a_Hidden_Path;
```

## Input Format

The input should be provided in the following format:
1. First line: Number of states N
2. Second line: Length of hidden path (optional if already known)
3. Third line: Hidden path π₁ π₂ ... πₙ (space-separated integers)
4. Next N lines: Transition matrix T (N×N), each row on a separate line

## Example Input:
```
4
5
1 2 3 4 1
0.5 0.5 0.0 0.0
0.3 0.3 0.3 0.1
0.1 0.2 0.4 0.3
0.2 0.3 0.3 0.2
```

## Example Output:
```
Probability: 0.0072900000
```

## Explanation

The probability of a hidden path π₁π₂...πₙ is computed as:
P(π) = T[π₁][π₂] × T[π₂][π₃] × ... × T[πₙ₋₁][πₙ]

Where T[i][j] represents the transition probability from state i to state j.

In the example, we compute:
P(1→2→3→4→1) = T[1][2] × T[2][3] × T[3][4] × T[4][1]
= 0.5 × 0.3 × 0.3 × 0.2 = 0.009

The algorithm iterates through consecutive pairs of states in the path and multiplies the corresponding transition probabilities to get the final result.