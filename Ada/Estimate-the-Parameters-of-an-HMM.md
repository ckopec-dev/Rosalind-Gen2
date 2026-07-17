# Rosalind Problem: Estimate the Parameters of an HMM

## Problem Description
Given a set of observed sequences and a Hidden Markov Model structure, we need to estimate the transition probabilities, emission probabilities, and initial state probabilities.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Estimate_HMM_Parameters is
   
   -- Constants
   MAX_STATES := 5;
   MAX_SYMBOLS := 26;
   MAX_SEQUENCES := 100;
   
   -- Types
   type State_Index is range 1..MAX_STATES;
   type Symbol_Index is range 1..MAX_SYMBOLS;
   type Sequence_Length is range 1..1000;
   
   type Transition_Matrix is array (State_Index, State_Index) of Float;
   type Emission_Matrix is array (State_Index, Symbol_Index) of Float;
   type Initial_Probabilities is array (State_Index) of Float;
   
   -- Input data
   Num_States : Integer;
   Num_Symbols : Integer;
   Sequences : array (1..MAX_SEQUENCES) of array (1..1000) of Symbol_Index;
   Sequence_Lengths : array (1..MAX_SEQUENCES) of Integer;
   Num_Sequences : Integer := 0;
   
   -- Estimated parameters
   Transition_Probabilities : Transition_Matrix;
   Emission_Probabilities : Emission_Matrix;
   Initial_Probabilities : Initial_Probabilities;
   
   -- Helper functions
   function Normalize_Vector(V : in out array (State_Index) of Float) return Float is
      Sum : Float := 0.0;
   begin
      for I in State_Index loop
         Sum := Sum + V(I);
      end loop;
      
      if Sum > 0.0 then
         for I in State_Index loop
            V(I) := V(I) / Sum;
         end loop;
      end if;
      
      return Sum;
   end Normalize_Vector;
   
   procedure Initialize_Parameters is
   begin
      -- Initialize all probabilities to small values (avoiding zero)
      for I in State_Index loop
         Initial_Probabilities(I) := 1.0 / Float(Num_States);
         for J in State_Index loop
            Transition_Probabilities(I, J) := 1.0 / Float(Num_States);
         end loop;
         for K in Symbol_Index loop
            Emission_Probabilities(I, K) := 1.0 / Float(Num_Symbols);
         end loop;
      end loop;
   end Initialize_Parameters;
   
   procedure Update_Estimates is
      -- Counters for MLE estimation
      Transition_Counts : array (State_Index, State_Index) of Float := (others => (others => 0.0));
      Emission_Counts : array (State_Index, Symbol_Index) of Float := (others => (others => 0.0));
      Initial_Counts : array (State_Index) of Float := (others => 0.0);
      
      -- Total counts for normalization
      State_Transition_Total : array (State_Index) of Float := (others => 0.0);
      State_Emission_Total : array (State_Index) of Float := (others => 0.0);
   begin
      -- For each sequence, compute counts
      for Seq_Index in 1..Num_Sequences loop
         declare
            Length : constant Integer := Sequence_Lengths(Seq_Index);
            Current_State : State_Index;
         begin
            -- Count initial state
            if Length > 0 then
               Initial_Counts(Sequences(Seq_Index)(1)) := Initial_Counts(Sequences(Seq_Index)(1)) + 1.0;
               Current_State := Sequences(Seq_Index)(1);
            end if;
            
            -- Count transitions and emissions
            for I in 2..Length loop
               declare
                  Prev_State : constant State_Index := Sequences(Seq_Index)(I-1);
                  Current_State : constant State_Index := Sequences(Seq_Index)(I);
                  Symbol : constant Symbol_Index := Sequences(Seq_Index)(I);
               begin
                  Transition_Counts(Prev_State, Current_State) := 
                     Transition_Counts(Prev_State, Current_State) + 1.0;
                  Emission_Counts(Current_State, Symbol) := 
                     Emission_Counts(Current_State, Symbol) + 1.0;
                  
                  State_Transition_Total(Prev_State) := State_Transition_Total(Prev_State) + 1.0;
                  State_Emission_Total(Current_State) := State_Emission_Total(Current_State) + 1.0;
               end;
            end loop;
         end;
      end loop;
      
      -- Update transition probabilities
      for I in State_Index loop
         for J in State_Index loop
            if State_Transition_Total(I) > 0.0 then
               Transition_Probabilities(I, J) := Transition_Counts(I, J) / State_Transition_Total(I);
            else
               Transition_Probabilities(I, J) := 1.0 / Float(Num_States);
            end if;
         end loop;
      end loop;
      
      -- Update emission probabilities
      for I in State_Index loop
         for J in Symbol_Index loop
            if State_Emission_Total(I) > 0.0 then
               Emission_Probabilities(I, J) := Emission_Counts(I, J) / State_Emission_Total(I);
            else
               Emission_Probabilities(I, J) := 1.0 / Float(Num_Symbols);
            end if;
         end loop;
      end loop;
      
      -- Update initial probabilities
      declare
         Total_Initial : Float := 0.0;
      begin
         for I in State_Index loop
            Total_Initial := Total_Initial + Initial_Counts(I);
         end loop;
         
         if Total_Initial > 0.0 then
            for I in State_Index loop
               Initial_Probabilities(I) := Initial_Counts(I) / Total_Initial;
            end loop;
         else
            for I in State_Index loop
               Initial_Probabilities(I) := 1.0 / Float(Num_States);
            end loop;
         end if;
      end;
   end Update_Estimates;
   
   procedure Print_Parameters is
   begin
      -- Print initial probabilities
      Put_Line("Initial Probabilities:");
      for I in State_Index loop
         Put(Initial_Probabilities(I), Fore => 1, Aft => 6, Exp => 0);
         Put(" ");
      end loop;
      New_Line;
      
      -- Print transition matrix
      Put_Line("Transition Matrix:");
      for I in State_Index loop
         for J in State_Index loop
            Put(Transition_Probabilities(I, J), Fore => 1, Aft => 6, Exp => 0);
            Put(" ");
         end loop;
         New_Line;
      end loop;
      
      -- Print emission matrix
      Put_Line("Emission Matrix:");
      for I in State_Index loop
         for J in Symbol_Index loop
            Put(Emission_Probabilities(I, J), Fore => 1, Aft => 6, Exp => 0);
            Put(" ");
         end loop;
         New_Line;
      end loop;
   end Print_Parameters;
   
begin
   -- Read input parameters
   Put_Line("Enter number of states:");
   Get(Num_States);
   
   Put_Line("Enter number of symbols:");
   Get(Num_Symbols);
   
   Put_Line("Enter number of sequences:");
   Get(Num_Sequences);
   
   -- Read sequences
   for I in 1..Num_Sequences loop
      Put_Line("Enter sequence " & Integer'image(I) & ":");
      declare
         Input_Line : String(1..1000);
         Length : Natural;
      begin
         Get_Line(Input_Line, Length);
         Sequence_Lengths(I) := Length;
         for J in 1..Length loop
            Sequences(I)(J) := Symbol_Index(Character'Pos(Input_Line(J)) - Character'Pos('A') + 1);
         end loop;
      end;
   end loop;
   
   -- Initialize parameters
   Initialize_Parameters;
   
   -- Perform parameter estimation (EM algorithm steps)
   -- For simplicity, we'll do one iteration of MLE
   Update_Estimates;
   
   -- Print results
   Put_Line("Estimated HMM Parameters:");
   Print_Parameters;
   
end Estimate_HMM_Parameters;
```

## Explanation

This Ada implementation solves the HMM parameter estimation problem by:

1. **Input Reading**: Reads the number of states, symbols, and sequences, along with the actual sequences.

2. **Initialization**: Initializes all probabilities to uniform distributions to avoid zero probabilities.

3. **Counting**: For each sequence, counts:
   - Initial state occurrences
   - State transitions between consecutive states
   - Emission of symbols from each state

4. **Parameter Update**: Uses Maximum Likelihood Estimation (MLE) to update:
   - Initial state probabilities (based on first symbol counts)
   - Transition probabilities (based on state transition counts)
   - Emission probabilities (based on symbol emission counts)

5. **Normalization**: Ensures all probability distributions sum to 1.

## Key Features

- **Modular Design**: Separate procedures for initialization, counting, updating, and output
- **Error Handling**: Uses default values when counts are zero
- **Proper Normalization**: Ensures probabilities sum to 1
- **Clean Output**: Formatted printing of all estimated parameters

The algorithm implements a simplified version of the Expectation-Maximization (EM) algorithm for HMM parameter estimation, specifically using MLE which is appropriate for this problem type.