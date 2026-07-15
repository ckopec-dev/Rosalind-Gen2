# Rosalind Problem: Compute the Probability of an Outcome Given a Hidden Path

This problem asks us to compute the probability of observing a sequence of outcomes given a hidden path through a Markov model.

## Problem Understanding

We are given:
- A hidden path (sequence of states)
- An emission matrix (probabilities of observing each symbol in each state)
- We need to calculate the probability of the observed sequence given the hidden path

## Solution Approach

The probability of an observation sequence given a hidden path is simply the product of emission probabilities for each observation in its corresponding state.

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Compute_Probability_of_an_Outcome_Given_a_Hidden_Path is
   
   -- Function to compute probability of observation sequence given hidden path
   function Compute_Probability(Observation : String; 
                               Path        : String; 
                               Emission    : array (Character, Character) of Float) 
                               return Float is
      Probability : Float := 1.0;
   begin
      -- For each position in the sequence
      for I in Observation'First .. Observation'Last loop
         -- Get the state from the path
         declare
            State : Character := Path(I);
            Symbol : Character := Observation(I);
         begin
            -- Multiply by emission probability
            Probability := Probability * Emission(State, Symbol);
         end;
      end loop;
      
      return Probability;
   end Compute_Probability;

begin
   -- Example input (you would read from files in practice)
   declare
      Observation : constant String := "AAACCCGGGTTTTT";
      Path        : constant String := "BABBABABABABAB";
      
      -- Emission matrix: Emission(State, Symbol) = probability
      Emission : array (Character, Character) of Float :=
        (('A', 0.176), ('C', 0.225), ('G', 0.429), ('T', 0.168),
         ('A', 0.225), ('C', 0.324), ('G', 0.125), ('T', 0.275),
         ('A', 0.429), ('C', 0.125), ('G', 0.225), ('T', 0.168),
         ('A', 0.168), ('C', 0.275), ('G', 0.168), ('T', 0.394));
   begin
      -- Note: This is a simplified version - in practice you'd have proper matrix indexing
      Put_Line("Probability: " & Float'Image(Compute_Probability(Observation, Path, Emission)));
   end;
end Compute_Probability_of_an_Outcome_Given_a_Hidden_Path;
```

## More Realistic Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Compute_Probability_of_an_Outcome_Given_a_Hidden_Path is
   
   type State is (S1, S2, S3, S4);  -- A, C, G, T
   type Symbol is (A, C, G, T);
   
   type Emission_Matrix is array (State, Symbol) of Float;
   
   -- Function to convert character to state
   function Char_To_State(C : Character) return State is
   begin
      case C is
         when 'A' => return S1;
         when 'C' => return S2;
         when 'G' => return S3;
         when 'T' => return S4;
         when others => return S1;  -- Default
      end case;
   end Char_To_State;
   
   -- Function to convert character to symbol
   function Char_To_Symbol(C : Character) return Symbol is
   begin
      case C is
         when 'A' => return A;
         when 'C' => return C;
         when 'G' => return G;
         when 'T' => return T;
         when others => return A;  -- Default
      end case;
   end Char_To_Symbol;
   
   -- Function to compute probability of observation sequence given hidden path
   function Compute_Probability(Observation : String; 
                               Path        : String; 
                               Emission    : Emission_Matrix) 
                               return Float is
      Probability : Float := 1.0;
   begin
      -- For each position in the sequence
      for I in Observation'First .. Observation'Last loop
         -- Get the state from the path and symbol from observation
         declare
            State : State := Char_To_State(Path(I));
            Symbol : Symbol := Char_To_Symbol(Observation(I));
         begin
            -- Multiply by emission probability
            Probability := Probability * Emission(State, Symbol);
         end;
      end loop;
      
      return Probability;
   end Compute_Probability;

begin
   -- Example usage with sample data
   declare
      Observation : constant String := "AAACCCGGGTTTTT";
      Path        : constant String := "BABBABABABABAB";
      
      -- Emission matrix (in practice, this would be read from input)
      Emission : constant Emission_Matrix :=
        ((0.176, 0.225, 0.429, 0.168),  -- State S1 ('A')
         (0.225, 0.324, 0.125, 0.275),  -- State S2 ('C')
         (0.429, 0.125, 0.225, 0.168),  -- State S3 ('G')
         (0.168, 0.275, 0.168, 0.394)); -- State S4 ('T')
      
      Result : Float;
   begin
      Result := Compute_Probability(Observation, Path, Emission);
      Put_Line("Probability: " & Float'Image(Result));
   end;
end Compute_Probability_of_an_Outcome_Given_a_Hidden_Path;
```

## Key Points

1. **Input**: Hidden path (sequence of states) and observation sequence
2. **Process**: Multiply emission probabilities for each state-symbol pair
3. **Output**: The joint probability of the observation given the path
4. **Time Complexity**: O(n) where n is the length of the sequence
5. **Space Complexity**: O(1) additional space

This implementation computes P(observation | hidden_path) by multiplying the emission probabilities for each position in the sequence, where each emission probability is P(symbol|state).