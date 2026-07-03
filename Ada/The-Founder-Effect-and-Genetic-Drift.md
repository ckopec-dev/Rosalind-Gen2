# Rosalind Problem: The_Founder_Effect_and_Genetic_Drift

## Problem Understanding

This problem involves simulating genetic drift in a population, specifically focusing on the founder effect where a small group of individuals establishes a new population with reduced genetic diversity.

## Solution Approach

I'll create an Ada program that:
1. Simulates genetic drift in a population over multiple generations
2. Tracks allele frequencies
3. Demonstrates the founder effect by starting with a small founding population
4. Shows how genetic drift reduces variation over time

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Command_Line; use Ada.Command_Line;

procedure The_Founder_Effect_and_Genetic_Drift is
   
   -- Population parameters
   type Allele is (A, a);
   type Population is array (Positive range <>) of Allele;
   
   -- Simulation parameters
   type Parameters is record
      Population_Size : Positive;
      Generations     : Positive;
      Founder_Size    : Positive;
      Initial_Frequency : Float;
   end record;
   
   -- Function to generate a random allele based on frequency
   function Random_Allele(Freq : Float) return Allele is
   begin
      if Ada.Numerics.Float_Random < Freq then
         return A;
      else
         return a;
      end if;
   end Random_Allele;
   
   -- Function to calculate allele frequency in population
   function Get_Frequency(Pop : Population) return Float is
      Count : Natural := 0;
   begin
      for I in Pop'Range loop
         if Pop(I) = A then
            Count := Count + 1;
         end if;
      end loop;
      return Float(Count) / Float(Pop'Length);
   end Get_Frequency;
   
   -- Function to simulate one generation of genetic drift
   function Next_Generation(Pop : Population) return Population is
      New_Pop : Population(Pop'Range);
   begin
      for I in Pop'Range loop
         New_Pop(I) := Random_Allele(Get_Frequency(Pop));
      end loop;
      return New_Pop;
   end Next_Generation;
   
   -- Function to simulate founder effect and genetic drift
   procedure Simulate_Founder_Effect(P : Parameters) is
      Current_Pop : Population(1..P.Population_Size);
      Frequency   : Float;
      Generation  : Positive := 1;
      
      -- Initialize founder population
      procedure Initialize_Population is
      begin
         for I in Current_Pop'Range loop
            Current_Pop(I) := Random_Allele(P.Initial_Frequency);
         end loop;
      end Initialize_Population;
      
   begin
      Put_Line("Founder Effect Simulation:");
      Put_Line("Initial population size: " & Positive'Image(P.Population_Size));
      Put_Line("Founder size: " & Positive'Image(P.Founder_Size));
      Put_Line("Initial allele frequency: " & Float'Image(P.Initial_Frequency));
      Put_Line("Number of generations: " & Positive'Image(P.Generations));
      Put_Line("------------------------");
      
      -- Initialize founder population
      Initialize_Population;
      
      -- Print initial frequency
      Frequency := Get_Frequency(Current_Pop);
      Put("Generation 0: ");
      Put(Frequency, Fore => 1, Aft => 4, Exp => 0);
      Put_Line(" (allele A)");
      
      -- Simulate generations of drift
      for G in 1..P.Generations loop
         Current_Pop := Next_Generation(Current_Pop);
         Frequency := Get_Frequency(Current_Pop);
         
         if G mod 10 = 0 or G = P.Generations then
            Put("Generation " & Positive'Image(G) & ": ");
            Put(Frequency, Fore => 1, Aft => 4, Exp => 0);
            Put_Line(" (allele A)");
         end if;
      end loop;
      
   end Simulate_Founder_Effect;
   
   -- Main simulation with default parameters
   procedure Run_Simulation is
      Params : Parameters := (
         Population_Size => 100,
         Generations     => 100,
         Founder_Size    => 10,
         Initial_Frequency => 0.5
      );
   begin
      Put_Line("Genetic Drift and Founder Effect Simulation");
      Put_Line("==========================================");
      
      -- Run simulation with different initial conditions
      Params.Initial_Frequency := 0.8;
      Simulate_Founder_Effect(Params);
      
      New_Line;
      
      Params.Initial_Frequency := 0.2;
      Simulate_Founder_Effect(Params);
      
      New_Line;
      
      Params.Initial_Frequency := 0.5;
      Params.Population_Size := 50;
      Params.Generations := 50;
      Simulate_Founder_Effect(Params);
   end Run_Simulation;

begin
   -- Set random seed for reproducibility (optional)
   Ada.Numerics.Float_Random.Reset;
   
   -- Run the main simulation
   Run_Simulation;
   
end The_Founder_Effect_and_Genetic_Drift;
```

## Key Concepts Demonstrated

1. **Founder Effect**: Starting with a small population that establishes a new gene pool
2. **Genetic Drift**: Random changes in allele frequencies over generations
3. **Allele Frequency Tracking**: Monitoring how often each allele appears in the population
4. **Population Size Effects**: How smaller populations experience more dramatic drift effects

## Expected Output Format

The program will output:
- Initial conditions (population size, founder size, initial frequency)
- Allele frequency at selected generations
- Demonstration of how genetic drift reduces diversity over time

## Compilation and Execution

```bash
gnatmake the_founder_effect_and_genetic_drift.adb
./the_founder_effect_and_genetic_drift
```

This Ada program simulates the founder effect and genetic drift, showing how small populations experience more rapid changes in allele frequencies due to random sampling effects.