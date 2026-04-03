# Rosalind Problem: Solve_the_Soft_Decoding_Problem

## Problem Understanding

The soft decoding problem involves computing the posterior probabilities of hidden states given observed sequences and a Hidden Markov Model (HMM). We need to calculate the probability that a particular hidden state was active at a specific time point, given the entire observation sequence.

## Solution Approach

I'll implement the forward-backward algorithm to compute these posterior probabilities, which is the standard approach for soft decoding in HMMs.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Solve_The_Soft_Decoding_Problem is
   type State is (A, B);
   type Observation is (x, y, z);
   
   -- HMM parameters
   -- Initial probabilities
   Pi : array (State) of Float := (0.5, 0.5);
   
   -- Transition probabilities
   T : array (State, State) of Float := 
     ((0.7, 0.3),  -- From A to A, B
      (0.4, 0.6)); -- From B to A, B
   
   -- Emission probabilities
   E : array (State, Observation) of Float := 
     ((0.2, 0.3, 0.5),  -- State A emits x, y, z
      (0.5, 0.4, 0.1)); -- State B emits x, y, z
   
   -- Observation sequence
   Observations : array (1..5) of Observation := (x, y, z, x, y);
   
   -- Forward variables
   Alpha : array (1..5, State) of Float;
   
   -- Backward variables
   Beta : array (1..5, State) of Float;
   
   -- Normalization factors
   C : array (1..5) of Float;
   
   -- Computed posterior probabilities
   Posterior : array (1..5, State) of Float;
   
   -- Forward algorithm
   procedure Forward is
      procedure Normalize (t : Integer) is
         Sum : Float := 0.0;
      begin
         for i in State loop
            Sum := Sum + Alpha(t, i);
         end loop;
         C(t) := Sum;
         for i in State loop
            Alpha(t, i) := Alpha(t, i) / Sum;
         end loop;
      end Normalize;
   begin
      -- Initialization
      for i in State loop
         Alpha(1, i) := Pi(i) * E(i, Observations(1));
      end loop;
      Normalize(1);
      
      -- Recursion
      for t in 2..5 loop
         for j in State loop
            Alpha(t, j) := 0.0;
            for i in State loop
               Alpha(t, j) := Alpha(t, j) + Alpha(t-1, i) * T(i, j);
            end loop;
            Alpha(t, j) := Alpha(t, j) * E(j, Observations(t));
         end loop;
         Normalize(t);
      end loop;
   end Forward;
   
   -- Backward algorithm
   procedure Backward is
   begin
      -- Initialization
      for i in State loop
         Beta(5, i) := 1.0 / C(5);
      end loop;
      
      -- Recursion
      for t in reverse 1..4 loop
         for i in State loop
            Beta(t, i) := 0.0;
            for j in State loop
               Beta(t, i) := Beta(t, i) + T(i, j) * E(j, Observations(t+1)) * Beta(t+1, j);
            end loop;
            Beta(t, i) := Beta(t, i) / C(t);
         end loop;
      end loop;
   end Backward;
   
   -- Compute posterior probabilities
   procedure Compute_Posterior is
   begin
      for t in 1..5 loop
         for i in State loop
            Posterior(t, i) := Alpha(t, i) * Beta(t, i);
         end loop;
      end loop;
   end Compute_Posterior;
   
begin
   -- Execute algorithms
   Forward;
   Backward;
   Compute_Posterior;
   
   -- Output results
   Put_Line("Posterior probabilities for each state at each time point:");
   Put_Line("Time | State A | State B");
   Put_Line("-----|---------|--------");
   
   for t in 1..5 loop
      Put(t, Width => 4);
      Put(" | ");
      Put(Posterior(t, A), Aft => 4, Exp => 0);
      Put(" | ");
      Put(Posterior(t, B), Aft => 4, Exp => 0);
      New_Line;
   end loop;
   
end Solve_The_Soft_Decoding_Problem;
```

## Explanation

This Ada implementation solves the soft decoding problem using the forward-backward algorithm:

1. **Forward Algorithm**: Computes the forward variables α(t,i) which represent the probability of being in state i at time t, given the observations up to time t.

2. **Backward Algorithm**: Computes the backward variables β(t,i) which represent the probability of the observations from time t+1 to T, given that we are in state i at time t.

3. **Posterior Probability**: The final posterior probability P(i_t = i | observations) is computed as the product of α(t,i) and β(t,i), normalized appropriately.

## Key Features

- Uses proper Ada array declarations with named types for states and observations
- Implements the standard forward-backward algorithm with normalization
- Handles proper probability calculations with floating-point arithmetic
- Outputs results in a clear tabular format
- Includes proper procedure organization and modular design

The solution computes the posterior probabilities for each hidden state at each time point in the observation sequence, which is the core requirement of the soft decoding problem.

