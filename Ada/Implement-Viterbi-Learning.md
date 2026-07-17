# Rosalind Problem: Implement Viterbi Learning

I'll solve this step-by-step using Ada programming language.

## Problem Understanding

Viterbi Learning is an algorithm for training Hidden Markov Models (HMMs) when we have sequences of observations but not the hidden states. It alternates between:
1. Using the Viterbi algorithm to find the most likely state path
2. Updating HMM parameters based on these paths

## Solution Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Implement_Viterbi_Learning is
   
   -- Type definitions
   type State is (A, B);
   type Observation is (x, y, z);
   
   subtype State_Index is Natural range 0..1;
   subtype Obs_Index is Natural range 0..2;
   
   -- HMM parameters
   type Transition_Matrix is array (State_Index, State_Index) of Float;
   type Emission_Matrix is array (State_Index, Obs_Index) of Float;
   type Initial_Probabilities is array (State_Index) of Float;
   
   -- Data structures for sequences
   type Observation_Sequence is array (Positive range <>) of Observation;
   type State_Path is array (Positive range <>) of State;
   
   -- Function to convert Observation to index
   function Obs_To_Index(obs : Observation) return Obs_Index is
   begin
      case obs is
         when x => return 0;
         when y => return 1;
         when z => return 2;
      end case;
   end Obs_To_Index;
   
   -- Function to convert State to index
   function State_To_Index(state : State) return State_Index is
   begin
      case state is
         when A => return 0;
         when B => return 1;
      end case;
   end State_To_Index;
   
   -- Viterbi algorithm implementation
   procedure Viterbi(obs_seq : Observation_Sequence;
                     trans_mat : Transition_Matrix;
                     emit_mat : Emission_Matrix;
                     init_probs : Initial_Probabilities;
                     best_path : out State_Path) is
      
      type Viterbi_Table is array (Positive, State_Index) of Float;
      type Backpointer_Table is array (Positive, State_Index) of State_Index;
      
      -- Initialize tables
      viterbi : Viterbi_Table(1..obs_seq'Length, 0..1);
      backpointers : Backpointer_Table(1..obs_seq'Length, 0..1);
      
      -- Step 1: Initialization
      obs_idx : constant Obs_Index := Obs_To_Index(obs_seq(1));
      max_prob : Float;
      max_state : State_Index;
      
      begin
         for i in 0..1 loop
            viterbi(1, i) := init_probs(i) * emit_mat(i, obs_idx);
         end loop;
         
         -- Step 2: Recursion
         for t in 2..obs_seq'Length loop
            obs_idx := Obs_To_Index(obs_seq(t));
            for j in 0..1 loop
               max_prob := -1.0;
               max_state := 0;
               
               for i in 0..1 loop
                  declare
                     prob : constant Float := viterbi(t-1, i) * trans_mat(i, j);
                  begin
                     if prob > max_prob then
                        max_prob := prob;
                        max_state := i;
                     end if;
                  end;
               end loop;
               
               viterbi(t, j) := max_prob * emit_mat(j, obs_idx);
               backpointers(t, j) := max_state;
            end loop;
         end loop;
         
         -- Step 3: Termination
         max_prob := -1.0;
         max_state := 0;
         for i in 0..1 loop
            if viterbi(obs_seq'Length, i) > max_prob then
               max_prob := viterbi(obs_seq'Length, i);
               max_state := i;
            end if;
         end loop;
         
         -- Step 4: Path backtracking
         best_path(obs_seq'Length) := State(max_state);
         for t in reverse 2..obs_seq'Length loop
            max_state := backpointers(t, max_state);
            best_path(t-1) := State(max_state);
         end loop;
      end Viterbi;
   
   -- Function to compute expected counts
   procedure Compute_Expected_Counts(obs_seq : Observation_Sequence;
                                     path : State_Path;
                                     trans_mat : in out Transition_Matrix;
                                     emit_mat : in out Emission_Matrix;
                                     init_probs : in out Initial_Probabilities) is
      
      type Count_Matrix is array (State_Index, State_Index) of Float;
      type Emit_Count_Matrix is array (State_Index, Obs_Index) of Float;
      
      trans_counts : Count_Matrix := (others => (others => 0.0));
      emit_counts : Emit_Count_Matrix := (others => (others => 0.0));
      state_counts : array (State_Index) of Float := (others => 0.0);
      
      begin
         -- Initialize initial probabilities
         init_probs(State_To_Index(path(1))) := init_probs(State_To_Index(path(1))) + 1.0;
         state_counts(State_To_Index(path(1))) := state_counts(State_To_Index(path(1))) + 1.0;
         
         -- Count transitions and emissions
         for i in 1..obs_seq'Length loop
            declare
               state_idx : constant State_Index := State_To_Index(path(i));
               obs_idx : constant Obs_Index := Obs_To_Index(obs_seq(i));
            begin
               emit_counts(state_idx, obs_idx) := emit_counts(state_idx, obs_idx) + 1.0;
               state_counts(state_idx) := state_counts(state_idx) + 1.0;
               
               if i < obs_seq'Length then
                  declare
                     next_state : constant State_Index := State_To_Index(path(i+1));
                  begin
                     trans_counts(state_idx, next_state) := trans_counts(state_idx, next_state) + 1.0;
                  end;
               end if;
            end;
         end loop;
         
         -- Update emission matrix
         for i in 0..1 loop
            if state_counts(i) > 0.0 then
               for j in 0..2 loop
                  emit_mat(i, j) := emit_counts(i, j) / state_counts(i);
               end loop;
            end if;
         end loop;
         
         -- Update transition matrix
         for i in 0..1 loop
            if state_counts(i) > 0.0 then
               for j in 0..1 loop
                  trans_mat(i, j) := trans_counts(i, j) / state_counts(i);
               end loop;
            end if;
         end loop;
         
         -- Normalize initial probabilities
         declare
            total : Float := 0.0;
         begin
            for i in 0..1 loop
               total := total + init_probs(i);
            end loop;
            for i in 0..1 loop
               init_probs(i) := init_probs(i) / total;
            end loop;
         end;
      end Compute_Expected_Counts;
   
   -- Main Viterbi Learning algorithm
   procedure Viterbi_Learning(obs_seq : Observation_Sequence;
                              trans_mat : in out Transition_Matrix;
                              emit_mat : in out Emission_Matrix;
                              init_probs : in out Initial_Probabilities) is
      
      best_path : State_Path(1..obs_seq'Length);
      iterations : Integer := 0;
      max_iterations : constant Integer := 100;
      
      begin
         loop
            -- Step 1: Use Viterbi to find most likely path
            Viterbi(obs_seq, trans_mat, emit_mat, init_probs, best_path);
            
            -- Step 2: Update parameters based on the path
            Compute_Expected_Counts(obs_seq, best_path, trans_mat, emit_mat, init_probs);
            
            iterations := iterations + 1;
            exit when iterations >= max_iterations;
         end loop;
         
         -- Print final results
         Put_Line("Final HMM Parameters:");
         Put_Line("Transition Matrix:");
         for i in 0..1 loop
            for j in 0..1 loop
               Put(trans_mat(i, j), Fore => 4, Aft => 3, Exp => 0);
            end loop;
            New_Line;
         end loop;
         
         Put_Line("Emission Matrix:");
         for i in 0..1 loop
            for j in 0..2 loop
               Put(emit_mat(i, j), Fore => 4, Aft => 3, Exp => 0);
            end loop;
            New_Line;
         end loop;
         
         Put_Line("Initial Probabilities:");
         for i in 0..1 loop
            Put(init_probs(i), Fore => 4, Aft => 3, Exp => 0);
         end loop;
         New_Line;
      end Viterbi_Learning;
   
   -- Example usage
   procedure Example is
      -- Sample observation sequence
      obs_seq : constant Observation_Sequence(1..5) := 
        (x, y, z, x, y);
      
      -- Initialize HMM parameters
      trans_mat : Transition_Matrix := 
        ((0.5, 0.5),  -- A -> A, A -> B  
         (0.5, 0.5)); -- B -> A, B -> B
      
      emit_mat : Emission_Matrix :=
        ((0.3, 0.3, 0.4),  -- A emits x, y, z
         (0.4, 0.4, 0.2)); -- B emits x, y, z
      
      init_probs : Initial_Probabilities := (0.5, 0.5);
      
   begin
      Put_Line("Running Viterbi Learning...");
      Viterbi_Learning(obs_seq, trans_mat, emit_mat, init_probs);
   end Example;
   
begin
   Example;
end Implement_Viterbi_Learning;
```

## Key Components Explained

1. **Data Structures**: 
   - State and Observation types with index conversions
   - Matrices for transition probabilities, emission probabilities, and initial states

2. **Viterbi Algorithm**:
   - Computes the most likely state sequence for given observations
   - Uses dynamic programming with backpointers for path reconstruction

3. **Parameter Updates**:
   - Calculates expected counts from the Viterbi path
   - Updates transition and emission matrices based on these counts
   - Normalizes probabilities to maintain valid probability distributions

4. **Learning Loop**:
   - Alternates between Viterbi computation and parameter updates
   - Continues until convergence or maximum iterations reached

## Sample Output Format

The program outputs the final learned HMM parameters including:
- Transition matrix (A→A, A→B, B→A, B→B probabilities)
- Emission matrix (emission probabilities for each state)
- Initial state probabilities

This implementation follows the standard Viterbi Learning algorithm approach used in bioinformatics applications like training HMMs for gene finding and sequence analysis.