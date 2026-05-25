# Rosalind Problem: Implement Baum-Welch Learning in Ada

## Problem Understanding

The Baum-Welch algorithm is used to learn the parameters of a Hidden Markov Model (HMM) from observation sequences. Given an HMM with unknown parameters and a set of observation sequences, we need to estimate:
1. Transition probabilities between hidden states
2. Emission probabilities for observations
3. Initial state probabilities

## Solution Approach

We'll implement the Baum-Welch algorithm using the Expectation-Maximization (EM) approach, which iteratively improves the HMM parameters until convergence.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Command_Line; use Ada.Command_Line;

procedure Implement_Baum_Welch_Learning is
   
   -- Constants
   type State is (S1, S2, S3);  -- Hidden states
   type Observation is (O1, O2, O3);  -- Observations
   type Matrix is array (State, State) of Float;
   type Emission_Matrix is array (State, Observation) of Float;
   type Initial_Prob is array (State) of Float;
   
   -- HMM parameters
   type HMM is record
      transition : Matrix;
      emission   : Emission_Matrix;
      initial    : Initial_Prob;
   end record;
   
   -- Global variables
   num_states : constant := 3;
   num_observations : constant := 3;
   max_iterations : constant := 1000;
   tolerance : constant := 1.0e-6;
   
   -- Forward-backward algorithm
   function Forward(observations : array of Observation; 
                   hmm : HMM) return Matrix is
      alpha : Matrix;
      n : constant Integer := observations'Length;
   begin
      -- Initialize alpha
      for i in State loop
         alpha(i, 1) := hmm.initial(i) * hmm.emission(i, observations(1));
      end loop;
      
      -- Forward algorithm
      for t in 2..n loop
         for i in State loop
            alpha(i, t) := 0.0;
            for j in State loop
               alpha(i, t) := alpha(i, t) + alpha(j, t-1) * hmm.transition(j, i);
            end loop;
            alpha(i, t) := alpha(i, t) * hmm.emission(i, observations(t));
         end loop;
      end loop;
      
      return alpha;
   end Forward;
   
   function Backward(observations : array of Observation; 
                    hmm : HMM) return Matrix is
      beta : Matrix;
      n : constant Integer := observations'Length;
   begin
      -- Initialize beta
      for i in State loop
         beta(i, n) := 1.0;
      end loop;
      
      -- Backward algorithm
      for t in reverse 1..n-1 loop
         for i in State loop
            beta(i, t) := 0.0;
            for j in State loop
               beta(i, t) := beta(i, t) + hmm.transition(i, j) * hmm.emission(j, observations(t+1)) * beta(j, t+1);
            end loop;
         end loop;
      end loop;
      
      return beta;
   end Backward;
   
   -- Baum-Welch update functions
   procedure Update_Transition(observations : array of Observation; 
                              hmm : in out HMM;
                              alpha : Matrix;
                              beta : Matrix) is
      n : constant Integer := observations'Length;
      gamma : Matrix;
      xi : array (State, State) of Float;
      sum_gamma : Float;
   begin
      -- Calculate gamma (posterior probabilities)
      for i in State loop
         for t in 1..n loop
            gamma(i, t) := alpha(i, t) * beta(i, t) / (alpha(i, t) * beta(i, t) + 0.000001);  -- Avoid division by zero
         end loop;
      end loop;
      
      -- Update transition probabilities
      for i in State loop
         for j in State loop
            xi(i, j) := 0.0;
            for t in 1..n-1 loop
               xi(i, j) := xi(i, j) + alpha(i, t) * hmm.transition(i, j) * hmm.emission(j, observations(t+1)) * beta(j, t+1);
            end loop;
            -- Normalize by sum of gamma for state i
            sum_gamma := 0.0;
            for t in 1..n-1 loop
               sum_gamma := sum_gamma + alpha(i, t) * beta(i, t);
            end loop;
            if sum_gamma > 0.0 then
               hmm.transition(i, j) := xi(i, j) / sum_gamma;
            end if;
         end loop;
      end loop;
   end Update_Transition;
   
   procedure Update_Emission(observations : array of Observation; 
                           hmm : in out HMM;
                           alpha : Matrix;
                           beta : Matrix) is
      n : constant Integer := observations'Length;
      gamma : Matrix;
      sum_gamma : Float;
   begin
      -- Calculate gamma (posterior probabilities)
      for i in State loop
         for t in 1..n loop
            gamma(i, t) := alpha(i, t) * beta(i, t) / (alpha(i, t) * beta(i, t) + 0.000001);  -- Avoid division by zero
         end loop;
      end loop;
      
      -- Update emission probabilities
      for i in State loop
         for k in Observation loop
            -- Sum of gamma for state i where observation is k
            sum_gamma := 0.0;
            for t in 1..n loop
               if observations(t) = k then
                  sum_gamma := sum_gamma + gamma(i, t);
               end if;
            end loop;
            
            -- Sum of gamma for all observations for state i
            declare
               total_gamma : Float := 0.0;
            begin
               for t in 1..n loop
                  total_gamma := total_gamma + gamma(i, t);
               end loop;
               if total_gamma > 0.0 then
                  hmm.emission(i, k) := sum_gamma / total_gamma;
               end if;
            end;
         end loop;
      end loop;
   end Update_Emission;
   
   procedure Update_Initial(observations : array of Observation; 
                          hmm : in out HMM;
                          alpha : Matrix;
                          beta : Matrix) is
      n : constant Integer := observations'Length;
      sum_alpha : Float;
   begin
      -- Update initial probabilities
      for i in State loop
         sum_alpha := 0.0;
         for t in 1..n loop
            sum_alpha := sum_alpha + alpha(i, t) * beta(i, t);
         end loop;
         hmm.initial(i) := sum_alpha;
      end loop;
      
      -- Normalize initial probabilities
      declare
         total : Float := 0.0;
      begin
         for i in State loop
            total := total + hmm.initial(i);
         end loop;
         if total > 0.0 then
            for i in State loop
               hmm.initial(i) := hmm.initial(i) / total;
            end loop;
         end if;
      end;
   end Update_Initial;
   
   -- Baum-Welch algorithm
   procedure Baum_Welch(observations : array of Observation; 
                       hmm : in out HMM) is
      alpha : Matrix;
      beta : Matrix;
      old_log_likelihood : Float := -1.0e30;
      new_log_likelihood : Float;
      iteration : Integer := 0;
   begin
      loop
         -- Forward and backward
         alpha := Forward(observations, hmm);
         beta := Backward(observations, hmm);
         
         -- Calculate log likelihood (simplified)
         new_log_likelihood := 0.0;
         for i in State loop
            new_log_likelihood := new_log_likelihood + alpha(i, observations'Length);
         end loop;
         new_log_likelihood := log(new_log_likelihood);
         
         -- Check convergence
         if iteration > 0 and then abs(new_log_likelihood - old_log_likelihood) < tolerance then
            exit;
         end if;
         
         old_log_likelihood := new_log_likelihood;
         iteration := iteration + 1;
         
         -- Update parameters
         Update_Transition(observations, hmm, alpha, beta);
         Update_Emission(observations, hmm, alpha, beta);
         Update_Initial(observations, hmm, alpha, beta);
         
         -- Limit iterations
         exit when iteration >= max_iterations;
      end loop;
   end Baum_Welch;
   
   -- Print HMM parameters
   procedure Print_HMM(hmm : HMM) is
   begin
      Put_Line("Initial probabilities:");
      for i in State loop
         Put(i'Img & " : ");
         Put(hmm.initial(i), Fore => 1, Aft => 6, Exp => 0);
         New_Line;
      end loop;
      
      Put_Line("Transition probabilities:");
      for i in State loop
         Put(i'Img & " -> ");
         for j in State loop
            Put(hmm.transition(i, j), Fore => 1, Aft => 6, Exp => 0);
            Put(" ");
         end loop;
         New_Line;
      end loop;
      
      Put_Line("Emission probabilities:");
      for i in State loop
         Put(i'Img & " -> ");
         for j in Observation loop
            Put(hmm.emission(i, j), Fore => 1, Aft => 6, Exp => 0);
            Put(" ");
         end loop;
         New_Line;
      end loop;
   end Print_HMM;
   
   -- Main program
begin
   Put_Line("Baum-Welch Learning Algorithm");
   Put_Line("=============================");
   
   -- Initialize HMM with random parameters
   declare
      hmm : HMM;
   begin
      -- Initialize random generator
      -- Set initial probabilities
      hmm.initial(S1) := 0.5;
      hmm.initial(S2) := 0.3;
      hmm.initial(S3) := 0.2;
      
      -- Set transition probabilities
      hmm.transition(S1, S1) := 0.7; hmm.transition(S1, S2) := 0.2; hmm.transition(S1, S3) := 0.1;
      hmm.transition(S2, S1) := 0.3; hmm.transition(S2, S2) := 0.5; hmm.transition(S2, S3) := 0.2;
      hmm.transition(S3, S1) := 0.2; hmm.transition(S3, S2) := 0.3; hmm.transition(S3, S3) := 0.5;
      
      -- Set emission probabilities
      hmm.emission(S1, O1) := 0.5; hmm.emission(S1, O2) := 0.4; hmm.emission(S1, O3) := 0.1;
      hmm.emission(S2, O1) := 0.3; hmm.emission(S2, O2) := 0.5; hmm.emission(S2, O3) := 0.2;
      hmm.emission(S3, O1) := 0.2; hmm.emission(S3, O2) := 0.3; hmm.emission(S3, O3) := 0.5;
      
      Put_Line("Initial HMM parameters:");
      Print_HMM(hmm);
      
      -- Simulate some observations
      -- Note: In real implementation, this would be from input file
      declare
         observations : array (1..10) of Observation := (O1, O2, O3, O1, O2, O1, O3, O2, O1, O3);
      begin
         Put_Line("Observations: ");
         for i in observations'Range loop
            Put(observations(i)'Img);
            Put(" ");
         end loop;
         New_Line;
         
         -- Run Baum-Welch
         Put_Line("Running Baum-Welch algorithm...");
         Baum_Welch(observations, hmm);
         
         Put_Line("Final HMM parameters:");
         Print_HMM(hmm);
      end;
   end;
   
end Implement_Baum_Welch_Learning;
```

## Key Components

1. **Data Structures**:
   - State and Observation types
   - HMM record containing transition, emission, and initial probabilities
   - Matrix types for probability calculations

2. **Forward-Backward Algorithm**:
   - Calculates alpha (forward) and beta (backward) variables
   - Essential for computing posterior probabilities

3. **Parameter Updates**:
   - Transition probabilities updated using expected counts
   - Emission probabilities updated based on observation expectations
   - Initial probabilities updated from posterior probabilities

4. **Convergence Criteria**:
   - Iterates until log-likelihood changes are below tolerance
   - Maximum iterations to prevent infinite loops

## Usage Notes

This implementation provides a basic framework for Baum-Welch learning. For actual Rosalind problems, you would need to:
1. Parse input data from files
2. Handle multiple observation sequences
3. Implement proper input/output formatting
4. Add error handling for edge cases

The algorithm iteratively improves HMM parameters to maximize the likelihood of observed sequences, which is the core of unsupervised learning for HMMs.

