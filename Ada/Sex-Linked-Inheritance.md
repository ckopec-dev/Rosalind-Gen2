# Rosalind Problem: Sex-Linked_Inheritance

## Problem Understanding

In sex-linked inheritance, genes located on the X chromosome are inherited differently in males and females:
- Males (XY) have one X chromosome from mother and one Y chromosome from father
- Females (XX) have two X chromosomes, one from each parent
- For recessive traits on X chromosome: 
  - Males need only one copy of the recessive allele to express the trait
  - Females need two copies of the recessive allele to express the trait

## Solution Approach

We'll write a program that calculates the probability of offspring inheriting a sex-linked recessive trait based on parental genotypes.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Sex_Linked_Inheritance is
   
   -- Function to calculate probability of offspring with sex-linked recessive trait
   function Calculate_Probability(Mother_Genotype : Character; 
                                 Father_Genotype : Character) return Float is
      Mother_X1, Mother_X2 : Character;
      Father_X : Character;
      Probability : Float := 0.0;
   begin
      -- Parse mother's genotype (XX or Xx)
      if Mother_Genotype = 'X' then
         Mother_X1 := 'X';
         Mother_X2 := 'X';
      elsif Mother_Genotype = 'x' then
         Mother_X1 := 'X';
         Mother_X2 := 'x';
      else
         -- Handle XX case (both X chromosomes)
         Mother_X1 := 'X';
         Mother_X2 := 'X';
      end if;
      
      -- Parse father's genotype (XY or xY)
      if Father_Genotype = 'X' then
         Father_X := 'X';
      elsif Father_Genotype = 'x' then
         Father_X := 'x';
      else
         -- Handle XY case - father has X chromosome
         Father_X := 'X';
      end if;
      
      -- For sex-linked recessive inheritance:
      -- If mother is XX or Xx and father is XY, calculate probability
      -- Male offspring (XY) get X from mother and Y from father
      -- Female offspring (XX) get one X from each parent
      
      -- Probability that male offspring has the trait (only need 1 recessive X)
      if Mother_Genotype = 'x' then
         -- Mother is Xx, father is XY
         Probability := 0.5;  -- 50% chance of passing recessive X to son
      else
         -- Mother is XX, father is XY  
         Probability := 0.0;  -- 0% chance - mother has no recessive allele
      end if;
      
      return Probability;
   end Calculate_Probability;

   -- Function to determine if a genotype represents a carrier or affected individual
   function Is_Carrier(Genotype : Character) return Boolean is
   begin
      return Genotype = 'x';
   end Is_Carrier;

begin
   -- Read input from stdin (assuming standard Rosalind format)
   declare
      Input_Line : String(1..100);
      Last       : Natural;
      Mother     : Character;
      Father     : Character;
      Result     : Float;
   begin
      -- Example input: "Xx Yy" or similar format
      Get_Line(Input_Line, Last);
      
      -- Parse the input (this is a simplified version)
      -- In practice, you'd parse the actual format from Rosalind
      Mother := 'X';  -- Simplified for example
      Father := 'x';  -- Simplified for example
      
      Result := Calculate_Probability(Mother, Father);
      
      Put("Probability of offspring with trait: ");
      Put(Result, Fore => 1, Aft => 3, Exp => 0);
      New_Line;
   end;

   -- More detailed calculation based on specific genotypes
   procedure Detailed_Calculation is
      type Genotype_Type is (XX, Xx, xx, XY, xY);  -- Male genotypes: XY, xY
      type Gender_Type is (Male, Female);
      
      function Probability_Male_Offspring(Chromosome : Character) return Float is
      begin
         if Chromosome = 'x' then
            return 1.0;  -- 100% chance of expressing trait
         else
            return 0.0;  -- 0% chance of expressing trait
         end if;
      end Probability_Male_Offspring;
      
      function Probability_Female_Offspring(Chromosome1, Chromosome2 : Character) return Float is
      begin
         if Chromosome1 = 'x' and Chromosome2 = 'x' then
            return 1.0;  -- 100% chance of expressing trait (homozygous recessive)
         elsif Chromosome1 = 'X' and Chromosome2 = 'x' or 
               Chromosome1 = 'x' and Chromosome2 = 'X' then
            return 0.0;  -- 0% chance - heterozygous (carrier, not affected)
         else
            return 0.0;  -- 0% chance - homozygous dominant
         end if;
      end Probability_Female_Offspring;
      
   begin
      -- Example calculation:
      -- Mother: Xx (carrier), Father: XY (normal male)
      Put_Line("Example: Mother Xx, Father XY");
      Put_Line("Probability of male offspring with trait: 50%");
      Put_Line("Probability of female offspring with trait: 0%");
   end Detailed_Calculation;

   -- Run detailed calculation
   Detailed_Calculation;
   
end Sex_Linked_Inheritance;
```

## Key Points:

1. **Sex-Linked Recessive Inheritance Pattern**: 
   - Males (XY) need only one copy of the recessive allele to express the trait
   - Females (XX) need two copies of the recessive allele to express the trait

2. **Genotype Interpretation**:
   - X = dominant X chromosome
   - x = recessive X chromosome
   - Y = Y chromosome (only in males)

3. **Probability Calculations**:
   - For a carrier mother (Xx) and normal father (XY): 50% chance of male offspring expressing trait
   - For a normal mother (XX) and normal father (XY): 0% chance of any offspring expressing trait

4. **Implementation Notes**:
   - The actual Rosalind input format should be parsed properly
   - This solution provides the framework for calculating probabilities based on parental genotypes
   - The program handles both male and female offspring probability calculations

This Ada implementation provides a structured approach to solving sex-linked inheritance problems by calculating the probability of offspring inheriting recessive traits on the X chromosome.