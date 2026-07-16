# Rosalind Problem: Implement the Viterbi Algorithm in Ada

The Viterbi algorithm is used to find the most likely sequence of hidden states in a Hidden Markov Model (HMM). This implementation will solve the problem by finding the optimal hidden path given an observed sequence and HMM parameters.

## Problem Analysis

We need to implement the Viterbi algorithm that:
1. Takes an observed sequence
2. Takes HMM transition and emission probabilities
3. Returns the most likely sequence of hidden states

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Viterbi_Algorithm is
   
   -- Constants
   MAX_STATES := 100;
   MAX_OBSERVATIONS := 100;
   
   -- Type definitions
   type State is (S1, S2);  -- Two hidden states
   type Observation is (O1, O2, O3);  -- Three possible observations
   
   type Probability is delta 0.0001 range 0.0 .. 1.0;
   
   -- Transition matrix: trans(i,j) = P(state j | state i)
   type Trans_Matrix is array (State, State) of Probability;
   
   -- Emission matrix: emit(i,k) = P(observation k | state i)
   type Emit_Matrix is array (State, Observation) of Probability;
   
   -- Viterbi table for dynamic programming
   type Viterbi_Table is array (Integer range <>, State) of Probability;
   
   -- Backpointer table for traceback
   type Backpointer is array (Integer range <>, State) of State;
   
   -- Global data
   transition : Trans_Matrix;
   emission : Emit_Matrix;
   observations : array (Integer range 1..MAX_OBSERVATIONS) of Observation;
   num_states : Integer := 2;
   num_obs : Integer := 0;
   sequence_length : Integer := 0;
   
   -- Function to convert character to observation
   function Char_To_Obs(c : Character) return Observation is
   begin
      case c is
         when 'A' => return O1;
         when 'B' => return O2;
         when 'C' => return O3;
         when others => return O1;
      end case;
   end Char_To_Obs;
   
   -- Function to convert observation to character
   function Obs_To_Char(obs : Observation) return Character is
   begin
      case obs is
         when O1 => return 'A';
         when O2 => return 'B';
         when O3 => return 'C';
      end case;
   end Obs_To_Char;
   
   -- Initialize the HMM parameters
   procedure Initialize_HMM is
   begin
      -- Transition probabilities
      transition(S1, S1) := 0.5;
      transition(S1, S2) := 0.5;
      transition(S2, S1) := 0.5;
      transition(S2, S2) := 0.5;
      
      -- Emission probabilities
      emission(S1, O1) := 0.2;
      emission(S1, O2) := 0.4;
      emission(S1, O3) := 0.4;
      
      emission(S2, O1) := 0.5;
      emission(S2, O2) := 0.4;
      emission(S2, O3) := 0.1;
   end Initialize_HMM;
   
   -- Viterbi algorithm implementation
   procedure Viterbi(obs_seq : in String; 
                     viterbi_table : out Viterbi_Table;
                     backpointer : out Backpointer) is
      max_prob : Probability;
      max_state : State;
      
   begin
      sequence_length := obs_seq'Length;
      
      -- Initialize observations array
      for i in 1..sequence_length loop
         observations(i) := Char_To_Obs(obs_seq(i));
      end loop;
      
      -- Base case: initialization
      for s in State loop
         viterbi_table(1, s) := Probability(0.5) * emission(s, observations(1));
         backpointer(1, s) := S1;  -- Dummy value, will be overwritten
      end loop;
      
      -- Recursion step
      for t in 2..sequence_length loop
         for s in State loop
            max_prob := 0.0;
            max_state := S1;
            
            -- Find the maximum probability from previous states
            for prev_s in State loop
               declare
                  prob : Probability := viterbi_table(t-1, prev_s) * transition(prev_s, s);
               begin
                  if prob > max_prob then
                     max_prob := prob;
                     max_state := prev_s;
                  end if;
               end;
            end loop;
            
            -- Update Viterbi table
            viterbi_table(t, s) := max_prob * emission(s, observations(t));
            backpointer(t, s) := max_state;
         end loop;
      end loop;
   end Viterbi;
   
   -- Traceback to find the most likely path
   procedure Traceback(viterbi_table : in Viterbi_Table;
                      backpointer : in Backpointer;
                      path : out String) is
      max_prob : Probability := 0.0;
      max_state : State := S1;
      current_state : State;
      
   begin
      -- Find the state with maximum probability at the last time step
      for s in State loop
         if viterbi_table(sequence_length, s) > max_prob then
            max_prob := viterbi_table(sequence_length, s);
            max_state := s;
         end if;
      end loop;
      
      -- Trace back through the path
      current_state := max_state;
      for t in reverse 1..sequence_length loop
         path(t) := Obs_To_Char(current_state);
         current_state := backpointer(t, current_state);
      end loop;
   end Traceback;
   
   -- Main function to solve the problem
   procedure Solve_Viterbi is
      obs_seq : String(1..5) := "ABACA";
      viterbi_table : Viterbi_Table(1..5, State);
      backpointer : Backpointer(1..5, State);
      path : String(1..5);
      max_prob : Probability := 0.0;
      
   begin
      -- Initialize HMM parameters
      Initialize_HMM;
      
      -- Run Viterbi algorithm
      Viterbi(obs_seq, viterbi_table, backpointer);
      
      -- Find the maximum probability
      for s in State loop
         if viterbi_table(sequence_length, s) > max_prob then
            max_prob := viterbi_table(sequence_length, s);
         end if;
      end loop;
      
      -- Traceback to get the path
      Traceback(viterbi_table, backpointer, path);
      
      -- Output results
      Put_Line("Most likely hidden path:");
      Put_Line(path);
      Put_Line("Maximum probability: ");
      Put_Fixed(max_prob, 0, 4);
      New_Line;
      
   end Solve_Viterbi;
   
begin
   Solve_Viterbi;
end Viterbi_Algorithm;
```

## Key Features of the Implementation

1. **Type Definitions**: 
   - `State` and `Observation` types to represent hidden states and observations
   - `Probability` type with appropriate range for HMM probabilities

2. **HMM Parameters**:
   - Transition matrix showing probabilities between hidden states
   - Emission matrix showing probabilities of observations given states

3. **Viterbi Algorithm Steps**:
   - **Initialization**: Set base case probabilities
   - **Recursion**: Calculate maximum probability for each state at each time step
   - **Backtracking**: Trace the optimal path from the final state

4. **Data Structures**:
   - Viterbi table to store maximum probabilities
   - Backpointer table to track optimal previous states
   - Observation array to store input sequence

5. **Output**: The most likely sequence of hidden states for the given observation sequence

## Example Usage

The program is designed to solve a specific HMM problem with:
- Two hidden states (S1, S2)
- Three possible observations (A, B, C)
- Given transition and emission probabilities
- Input observation sequence "ABACA"

The output will show the most likely hidden state path that generated this observation sequence.