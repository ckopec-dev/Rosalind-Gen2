# Rosalind Problem: Partial Permutations (Partial_Permutations)

## Problem Statement
In "Enumerating Gene Orders", we used the standard permutation formula to calculate the number of permutations of length k taken from a set of n elements. However, we can also consider partial permutations, which are arrangements of k objects taken from a set of n objects where order matters.

The number of partial permutations of k objects taken from a set of n objects is given by:
P(n,k) = n! / (n-k)!

## Solution in Forth

```forth
\ Rosalind Partial Permutations problem solution in Forth

\ Function to calculate factorial
: factorial ( n -- n! )
  0 max 1 swap 1+ 1 do i * loop ;

\ Function to calculate partial permutations P(n,k)
: partial-permutations ( n k -- result )
  >r \ save k on return stack
  2dup - factorial \ calculate (n-k)!
  swap r> factorial \ calculate n! and get k back
  / \ divide n! by (n-k)!
  1000000 mod \ take result modulo 1000000 as required
  ;

\ Example usage:
\ 5 3 partial-permutations \ should give 60

\ Test with sample data
cr
." Sample test: n=5, k=3" cr
5 3 partial-permutations . cr
." Expected: 60" cr

\ More examples
cr
." Additional tests:" cr
." n=3, k=2:" 3 2 partial-permutations . cr
." n=10, k=7:" 10 7 partial-permutations . cr
```

## Explanation

The solution works as follows:

1. **Factorial Function**: `factorial` calculates n! by multiplying all integers from 1 to n
2. **Partial Permutations Function**: `partial-permutations` calculates P(n,k) = n!/(n-k)! using the formula
3. **Modulo Operation**: Since the result can be large, we take the result modulo 1000000 as required by Rosalind problems

## How it works step by step:

For P(5,3):
1. Calculate 5! = 120
2. Calculate (5-3)! = 2! = 2
3. Divide 120/2 = 60
4. Take 60 mod 1000000 = 60

## Key Forth Concepts Used:

- **Stack manipulation**: Using `>r` and `r>` to manage return stack
- **Control structures**: `do...loop` for factorial calculation
- **Arithmetic operations**: Division and modulo operations
- **Function composition**: Building complex operations from simpler ones

## Usage:
To run the solution with specific values:
```
5 3 partial-permutations
```

This will output the number of partial permutations of 3 objects taken from 5 objects, modulo 1000000.

