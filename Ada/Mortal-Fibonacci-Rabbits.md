# Rosalind Problem: Mortal Fibonacci Rabbits - Ada Solution

## Problem Understanding

In this problem, we need to calculate the number of rabbit pairs after `n` months, where:
- Each pair of rabbits produces a new pair every month starting from month 2
- Each pair lives for `m` months (dying after `m` months)
- We start with 1 pair in month 1

## Solution Approach

We'll use dynamic programming to track the population. The key insight is that we need to track rabbits by their age, since they die after `m` months.

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Mortal_Fibonacci_Rabbits is
   n : Integer;  -- Number of months
   m : Integer;  -- Lifespan in months
   total : Integer;
   
   -- Array to track rabbit pairs by age (0 to m-1)
   -- Age 0: Newborns (not yet breeding)
   -- Age 1 to m-1: Breeding rabbits
   type Rabbit_Age_Array is array (0 .. 20) of Long_Long_Integer;
   rabbits : Rabbit_Age_Array := (others => 0);
   
begin
   -- Read input
   Put("Enter number of months (n): ");
   Get(n);
   Put("Enter lifespan in months (m): ");
   Get(m);
   
   -- Initialize: 1 pair of newborns in month 1
   rabbits(0) := 1;
   
   -- Calculate population for each month
   for month in 2 .. n loop
      -- Shift ages: all rabbits age by 1 month
      for i in reverse 1 .. m-1 loop
         rabbits(i) := rabbits(i-1);
      end loop;
      
      -- Newborns = breeding rabbits from previous month (age 1 to m-2)
      rabbits(0) := 0;
      for i in 1 .. m-2 loop
         rabbits(0) := rabbits(0) + rabbits(i);
      end loop;
      
      -- Rabbits die at age m, so we reset that position
      rabbits(m-1) := 0;
   end loop;
   
   -- Calculate total population
   total := 0;
   for i in 0 .. m-1 loop
      total := total + rabbits(i);
   end loop;
   
   Put("Total rabbit pairs after ");
   Put(n);
   Put(" months: ");
   Put(total);
   New_Line;
   
end Mortal_Fibonacci_Rabbits;
```

## Alternative Implementation (More Clear)

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Mortal_Fibonacci_Rabbits is
   n : Integer;  -- Number of months
   m : Integer;  -- Lifespan in months
   total : Long_Long_Integer;
   
   -- Array to track rabbit pairs by age (0 to m-1)
   type Rabbit_Count_Array is array (0 .. 20) of Long_Long_Integer;
   rabbits : Rabbit_Count_Array := (others => 0);
   
begin
   -- Read input
   Put("Enter number of months (n): ");
   Get(n);
   Put("Enter lifespan in months (m): ");
   Get(m);
   
   -- Initialize: 1 pair of newborns in month 1
   rabbits(0) := 1;
   
   -- Calculate population for each month
   for month in 2 .. n loop
      -- Rabbits die at age m (index m-1), so they're reset to 0
      declare
         breeding_rabbits : Long_Long_Integer := 0;
      begin
         -- Count rabbits that can breed (age 1 to m-2)
         for i in 1 .. m-2 loop
            breeding_rabbits := breeding_rabbits + rabbits(i);
         end loop;
         
         -- Newborns = number of breeding rabbits from previous month
         rabbits(0) := breeding_rabbits;
         
         -- Age all rabbits by one month
         for i in reverse 1 .. m-1 loop
            rabbits(i) := rabbits(i-1);
         end loop;
         
         -- Reset the dying rabbits (age m)
         rabbits(m-1) := 0;
      end;
   end loop;
   
   -- Calculate total population
   total := 0;
   for i in 0 .. m-1 loop
      total := total + rabbits(i);
   end loop;
   
   Put("Total rabbit pairs after ");
   Put(n);
   Put(" months: ");
   Put(total);
   New_Line;
   
end Mortal_Fibonacci_Rabbits;
```

## Explanation

The algorithm works as follows:

1. **Initialization**: Start with 1 pair of newborn rabbits (age 0)
2. **Monthly Simulation**:
   - Each month, all rabbits age by one month
   - Rabbits that are `m` months old die and are reset to 0
   - Newborn rabbits are produced by all breeding rabbits (those aged 1 to m-2)
3. **Population Count**: Sum up all rabbit pairs across all ages

## Example Walkthrough

For n=6, m=3:
- Month 1: 1 pair (age 0)
- Month 2: 1 pair (age 1) 
- Month 3: 1 pair (age 2), 1 pair (age 0) = 2 pairs
- Month 4: 1 pair (age 0), 1 pair (age 1) = 2 pairs (old pair dies)
- And so on...

## Time Complexity
O(n × m) where n is the number of months and m is the lifespan.

## Space Complexity  
O(m) for storing rabbit counts by age.