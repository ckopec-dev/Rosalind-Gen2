# Rosalind Problem: Sex-Linked_Inheritance

## Problem Understanding

In sex-linked inheritance, traits are carried on the X chromosome. For a population with equal male and female individuals:
- Males have XY chromosomes (one X chromosome)
- Females have XX chromosomes (two X chromosomes)

We need to calculate the probability that a randomly selected individual from a population will carry a recessive X-linked trait.

## Solution Approach

Let's define:
- p = probability that a female carries the recessive trait (both X chromosomes are recessive)
- q = probability that a male carries the recessive trait (X chromosome is recessive)

For a population with equal numbers of males and females:
- Probability of selecting a female = 0.5
- Probability of selecting a male = 0.5

Total probability = 0.5 × p + 0.5 × q

## Forth Implementation

```forth
\ Sex-Linked Inheritance Problem Solution in Forth

\ Function to calculate probability of carrying recessive trait
\ Input: p (probability that female carries trait), q (probability that male carries trait)
\ Output: total probability
: sex-linked-prob ( p q -- prob )
    0.5e *          \ 0.5 * p
    0.5e *          \ 0.5 * q
    + ;             \ sum both probabilities

\ Alternative implementation using stack operations
: sex-linked-prob-alt ( p q -- prob )
    2dup            \ duplicate both values
    0.5e *          \ 0.5 * p
    swap 0.5e *     \ 0.5 * q
    + ;             \ add them together

\ Example usage:
\ If p = 0.2 (20% of females carry trait) and q = 0.1 (10% of males carry trait)
\ Then we expect: 0.5*0.2 + 0.5*0.1 = 0.15

\ Test with example values
: test-sex-linked ( -- )
    0.2e 0.1e sex-linked-prob . ;

\ More general function for multiple populations
: sex-linked-population ( p1 q1 p2 q2 -- total-prob )
    2dup            \ p1 q1 p2 q2 p1 q1
    0.5e *          \ p1 q1 p2 q2 p1*0.5
    swap 0.5e *     \ p1 q1 p2 q2 p1*0.5 q1*0.5
    +               \ p1 q1 p2 q2 (p1*0.5 + q1*0.5)
    2dup            \ p1 q1 p2 q2 (p1*0.5 + q1*0.5) (p1*0.5 + q1*0.5)
    0.5e *          \ p1 q1 p2 q2 (p1*0.5 + q1*0.5) (p2*0.5 + q2*0.5)
    swap 0.5e *     \ p1 q1 p2 q2 (p1*0.5 + q1*0.5) (p2*0.5 + q2*0.5)
    +               \ total probability
    ;

\ Simple version for single population
: single-sex-linked ( p q -- prob )
    0.5e *          \ 0.5 * p
    0.5e *          \ 0.5 * q
    + ;             \ sum
```

## Usage Example

```forth
\ Calculate probability for typical case
\ p = 0.2 (20% of females carry recessive trait)
\ q = 0.1 (10% of males carry recessive trait)
0.2e 0.1e single-sex-linked .  \ Output: 0.15

\ Calculate probability for a specific problem
\ If we have 0.3 for females and 0.05 for males
0.3e 0.05e single-sex-linked .  \ Output: 0.175
```

## Mathematical Explanation

The solution uses the law of total probability:

P(carrying trait) = P(carrying trait | female) × P(female) + P(carrying trait | male) × P(male)

Since we assume equal population sizes:
P(carrying trait) = 0.5 × p + 0.5 × q

Where:
- p = probability that a female individual carries the recessive trait
- q = probability that a male individual carries the recessive trait

## Sample Input-Output

Input:
```
p = 0.2
q = 0.1
```

Output:
```
0.15
```

This means there's a 15% chance that a randomly selected individual from this population carries the recessive X-linked trait.

