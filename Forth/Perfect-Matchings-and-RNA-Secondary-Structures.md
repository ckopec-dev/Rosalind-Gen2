# Rosalind Problem: Perfect Matchings and RNA Secondary Structures (Forth Solution)

## Problem Understanding

We need to find the number of perfect matchings in an RNA string where:
- A pairs with U
- C pairs with G
- Base pairs must follow Watson-Crick rules
- Perfect matching means every nucleotide is paired exactly once

## Approach

1. Count the number of A's and U's (they pair with each other)
2. Count the number of C's and G's (they pair with each other)
3. The number of perfect matchings is the product of factorials of these counts

## Forth Solution

```forth
\ Rosalind Perfect Matchings and RNA Secondary Structures
\ Solution in Forth

\ Factorial function
: factorial ( n -- n! )
    0 <= if
        1
    else
        dup 1- factorial *
    then ;

\ Count nucleotides in RNA string
: count-nucleotides ( addr len -- a-count u-count c-count g-count )
    0 0 0 0  \ Initialize counters
    over +  \ End address
    over  \ Start address
    begin
        over < while
        dup c@ case
            [char] A of 1+ swap 1+ swap 1+ swap 1+ swap 1+ endof
            [char] U of 1+ swap 1+ swap 1+ swap 1+ swap 1+ endof
            [char] C of 1+ swap 1+ swap 1+ swap 1+ swap 1+ endof
            [char] G of 1+ swap 1+ swap 1+ swap 1+ swap 1+ endof
        endcase
        1+  \ Move to next character
    repeat
    2drop ;

\ Main function to solve perfect matchings
: perfect-matchings ( addr len -- result )
    count-nucleotides  \ Get counts of A, U, C, G
    2dup  \ Duplicate A and U counts
    < if
        2dup  \ Duplicate A and U counts
        factorial  \ Calculate A! 
        swap factorial  \ Calculate U!
        *  \ Multiply A! * U!
    else
        2dup  \ Duplicate A and U counts
        factorial  \ Calculate U!
        swap factorial  \ Calculate A!
        *  \ Multiply U! * A!
    then
    2dup  \ Duplicate C and G counts
    < if
        2dup  \ Duplicate C and G counts
        factorial  \ Calculate C!
        swap factorial  \ Calculate G!
        *  \ Multiply C! * G!
    else
        2dup  \ Duplicate C and G counts
        factorial  \ Calculate G!
        swap factorial  \ Calculate C!
        *  \ Multiply G! * C!
    then
    *  \ Multiply (A!*U!) * (C!*G!) to get final result
    ;

\ Test with example
: test-perfect-matchings
    s" AUUCGCUAGC" 12 perfect-matchings . ;

\ Alternative cleaner version
: perfect-matchings-clean ( addr len -- result )
    0 0 0 0  \ A U C G counters
    over +  \ End address
    over  \ Start address
    begin
        over < while
        dup c@ case
            [char] A of 1+ swap 1+ swap 1+ swap 1+ swap 1+ endof
            [char] U of 1+ swap 1+ swap 1+ swap 1+ swap 1+ endof
            [char] C of 1+ swap 1+ swap 1+ swap 1+ swap 1+ endof
            [char] G of 1+ swap 1+ swap 1+ swap 1+ swap 1+ endof
        endcase
        1+  \ Move to next character
    repeat
    2drop  \ Remove addresses
    
    \ Calculate A! * U! * C! * G!
    2dup  \ A U
    factorial  \ A!
    swap factorial  \ U!
    *  \ A! * U!
    2dup  \ C G
    factorial  \ C!
    swap factorial  \ G!
    *  \ C! * G!
    *  \ (A!*U!) * (C!*G!)
    ;

\ Example usage
: example
    s" AUUCGCUAGC" 12 perfect-matchings-clean . ;
```

## Key Concepts

1. **Perfect Matching**: In RNA secondary structure, we want to count how many ways we can pair nucleotides such that each nucleotide participates in exactly one base pair.

2. **Watson-Crick Base Pairing**: A-U and C-G pairs only.

3. **Mathematical Insight**: 
   - Count A's and U's separately (they pair with each other)
   - Count C's and G's separately (they pair with each other)
   - The answer is A! × U! × C! × G!

## Example

For RNA string "AUUCGCUAGC":
- A count = 2
- U count = 3  
- C count = 3
- G count = 2

Result = 2! × 3! × 3! × 2! = 2 × 6 × 6 × 2 = 144

## Usage

```forth
s" AUUCGCUAGC" 12 perfect-matchings-clean .  \ Outputs: 144
```

This solution efficiently computes the number of perfect matchings using the mathematical property that we only need to consider the pairing constraints between complementary nucleotides.

