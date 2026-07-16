# Rosalind Problem: Compute the Probability of a String Emitted by an HMM

## Problem Understanding

We need to compute the probability that a Hidden Markov Model (HMM) emits a specific string. This is typically solved using the forward algorithm.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Compute_Probability_of_a_String_Emitted_by_an_HMM is
   
   type State is (A, B);
   type States is array (Positive range <>) of State;
   
   -- Transition probabilities
   T : array (State, State) of Float := 
     ((0.5, 0.5),  -- From A to A, B
      (0.3, 0.7));  -- From B to A, B
   
   -- Emission probabilities
   E : array (State, Character) of Float :=
     (('A' => 0.176, 'C' => 0.224, 'G' => 0.333, 'T' => 0.267),  -- State A
      ('A' => 0.317, 'C' => 0.183, 'G' => 0.317, 'T' => 0.183)); -- State B
   
   -- Initial probabilities (assuming equal probability for both states)
   Pi : array (State) of Float := (0.5, 0.5);
   
   -- Input string
   S : constant String := "ACGTACGT";
   
   procedure Forward_Algorithm is
      N : constant Natural := S'Length;
      Alpha : array (1..N, State) of Float;
      
      -- Initialize first row
      procedure Initialize_First_Row is
      begin
         for i in A..B loop
            Alpha(1, i) := Pi(i) * E(i, S(1));
         end loop;
      end Initialize_First_Row;
      
      -- Forward recursion
      procedure Forward_Recursion is
      begin
         for i in 2..N loop
            for j in A..B loop
               Alpha(i, j) := 0.0;
               for k in A..B loop
                  Alpha(i, j) := Alpha(i, j) + Alpha(i-1, k) * T(k, j);
               end loop;
               Alpha(i, j) := Alpha(i, j) * E(j, S(i));
            end loop;
         end loop;
      end Forward_Recursion;
      
   begin
      Initialize_First_Row;
      Forward_Recursion;
      
      -- Calculate final probability
      declare
         Final_Probability : Float := 0.0;
      begin
         for i in A..B loop
            Final_Probability := Final_Probability + Alpha(N, i);
         end loop;
         
         Put("Probability of string ");
         Put(S);
         Put(" being emitted: ");
         Put(Final_Probability, Fore => 1, Aft => 10, Exp => 0);
         New_Line;
      end;
   end Forward_Algorithm;
   
begin
   Forward_Algorithm;
end Compute_Probability_of_a_String_Emitted_by_an_HMM;
```

## Alternative Implementation (More Generic)

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Compute_Probability_of_a_String_Emitted_by_an_HMM is
   
   -- Define states and alphabet
   type State is (A, B);
   type Alphabet is ('A', 'C', 'G', 'T');
   
   -- HMM parameters
   type Transition_Matrix is array (State, State) of Float;
   type Emission_Matrix is array (State, Alphabet) of Float;
   type Initial_Probabilities is array (State) of Float;
   
   -- Example HMM parameters
   T : constant Transition_Matrix := 
     ((0.5, 0.5),  -- From A to A, B
      (0.3, 0.7));  -- From B to A, B
   
   E : constant Emission_Matrix :=
     (('A' => 0.176, 'C' => 0.224, 'G' => 0.333, 'T' => 0.267),  -- State A
      ('A' => 0.317, 'C' => 0.183, 'G' => 0.317, 'T' => 0.183)); -- State B
   
   Pi : constant Initial_Probabilities := (0.5, 0.5);
   
   -- Input string
   S : constant String := "ACGTACGT";
   
   function Forward_Probability(Sequence : String) return Float is
      N : constant Natural := Sequence'Length;
      Alpha : array (1..N, State) of Float;
      
      -- Initialize first row
      procedure Init_First_Row is
      begin
         for i in A..B loop
            Alpha(1, i) := Pi(i) * E(i, Alphabet(Sequence(1)));
         end loop;
      end Init_First_Row;
      
      -- Forward recursion
      procedure Forward_Recursion is
      begin
         for i in 2..N loop
            for j in A..B loop
               Alpha(i, j) := 0.0;
               for k in A..B loop
                  Alpha(i, j) := Alpha(i, j) + Alpha(i-1, k) * T(k, j);
               end loop;
               Alpha(i, j) := Alpha(i, j) * E(j, Alphabet(Sequence(i)));
            end loop;
         end loop;
      end Forward_Recursion;
      
   begin
      Init_First_Row;
      Forward_Recursion;
      
      -- Sum up final probabilities
      return Alpha(N, A) + Alpha(N, B);
   end Forward_Probability;
   
begin
   Put("Probability of string ");
   Put(S);
   Put(" being emitted: ");
   Put(Forward_Probability(S), Fore => 1, Aft => 10, Exp => 0);
   New_Line;
end Compute_Probability_of_a_String_Emitted_by_an_HMM;
```

## Explanation

This Ada solution implements the forward algorithm to compute the probability of a string being emitted by an HMM:

1. **Data Structures**: 
   - State enumeration (A, B)
   - Transition matrix T
   - Emission matrix E
   - Initial probabilities Pi

2. **Forward Algorithm Steps**:
   - Initialize first row with initial probabilities multiplied by emission probabilities
   - For each subsequent position, compute the probability of being in each state by summing over all possible previous states
   - Multiply by emission probability for current character
   - Sum final probabilities across all states

3. **Time Complexity**: O(N × |States|²) where N is string length
4. **Space Complexity**: O(N × |States|)

The algorithm efficiently computes the total probability that the given HMM would generate the specified sequence of characters.