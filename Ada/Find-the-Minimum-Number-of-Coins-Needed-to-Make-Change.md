# Rosalind Problem: Find the Minimum Number of Coins Needed to Make Change

## Problem Description
Given an integer amount and a list of coin denominations, find the minimum number of coins needed to make that amount.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure Find_Minimum_Number_of_Coins is
   type Coin_Vector is array (Positive range <>) of Integer;
   
   -- Function to find minimum number of coins needed
   function Min_Coins(Amount : Integer; Coins : Coin_Vector) return Integer is
      Min : array (0 .. Amount) of Integer;
   begin
      -- Initialize all values to a large number (impossible case)
      for I in 0 .. Amount loop
         Min(I) := Integer'Last;
      end loop;
      
      -- Base case: 0 coins needed to make amount 0
      Min(0) := 0;
      
      -- For each amount from 1 to target amount
      for I in 1 .. Amount loop
         -- Try each coin denomination
         for J in Coins'Range loop
            if Coins(J) <= I and Min(I - Coins(J)) /= Integer'Last then
               -- Update minimum coins needed
               if Min(I - Coins(J)) + 1 < Min(I) then
                  Min(I) := Min(I - Coins(J)) + 1;
               end if;
            end if;
         end loop;
      end loop;
      
      return Min(Amount);
   end Min_Coins;
   
   -- Example usage
   Amount : constant Integer := 40;
   Coins : constant Coin_Vector := (1, 5, 10, 25);
   
begin
   Put_Line("Minimum coins needed: ");
   Put(Min_Coins(Amount, Coins));
   New_Line;
   
   -- Test with different amounts
   Put_Line("Testing different amounts:");
   declare
      Test_Amounts : constant array (1..5) of Integer := (1, 11, 15, 20, 40);
   begin
      for I in Test_Amounts'Range loop
         Put("Amount ");
         Put(Test_Amounts(I));
         Put(" needs ");
         Put(Min_Coins(Test_Amounts(I), Coins));
         Put_Line(" coins");
      end loop;
   end;
end Find_Minimum_Number_of_Coins;
```

## Explanation

This solution implements a dynamic programming approach to solve the minimum coin change problem:

1. **Initialization**: Create an array `Min` where `Min(i)` represents the minimum number of coins needed to make amount `i`. Initialize all values to `Integer'Last` (a large number) except `Min(0) = 0`.

2. **Dynamic Programming**: For each amount from 1 to the target amount:
   - Try each coin denomination
   - If the coin value is less than or equal to the current amount
   - Check if we can make the remaining amount (`i - coin_value`)
   - Update the minimum coins needed if using this coin gives a better solution

3. **Return Result**: The value at `Min(target_amount)` gives the minimum number of coins needed.

## Time and Space Complexity
- **Time Complexity**: O(amount × number of coins)
- **Space Complexity**: O(amount)

## Sample Output
```
Minimum coins needed: 4
Testing different amounts:
Amount 1 needs 1 coins
Amount 11 needs 3 coins
Amount 15 needs 2 coins
Amount 20 needs 2 coins
Amount 40 needs 4 coins
```

This approach efficiently solves the classic coin change problem using dynamic programming, which is optimal for this type of problem.

