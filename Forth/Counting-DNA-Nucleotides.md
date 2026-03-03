# Rosalind Problem: Counting DNA Nucleotides - Forth Solution

## Problem Statement
Given a DNA string of length at most 1000 nucleotides, return four integers representing the counts of nucleotides A, C, G, and T respectively.

## Forth Solution

```forth
\ Counting DNA Nucleotides in Forth

: count-nucleotides ( addr len -- )
    0 0 0 0  \ Initialize counters for A, C, G, T
    0 do
        over i + c@  \ Get character at position i
        case
            [char] A of 1 + swap 1- swap endof
            [char] C of 1 + swap 1- swap endof
            [char] G of 1 + swap 1- swap endof
            [char] T of 1 + swap 1- swap endof
        endcase
    loop
    2drop  \ Remove the string address and length
    . . . .  \ Print the four counts
;

\ Alternative implementation using a more compact approach
: count-nucleotides-compact ( addr len -- )
    0 0 0 0  \ A C G T counters
    0 do
        over i + c@  \ Get character
        1+  \ Increment appropriate counter based on character
        case
            [char] A of 0 + 1- swap 1- swap 1- swap 1- swap endof
            [char] C of 1 + 1- swap 0 + 1- swap 1- swap 1- swap endof
            [char] G of 1 + 1- swap 1- swap 0 + 1- swap 1- swap endof
            [char] T of 1 + 1- swap 1- swap 1- swap 0 + 1- swap endof
        endcase
    loop
    2drop
    . . . .  \ Print results
;

\ Clean implementation - most readable version
: count-dna-nucleotides ( addr len -- )
    0 0 0 0  \ A C G T counters
    0 do
        over i + c@  \ Get character
        dup [char] A = if 1+ else drop then  \ Increment A counter
        dup [char] C = if 1+ else drop then  \ Increment C counter
        dup [char] G = if 1+ else drop then  \ Increment G counter
        dup [char] T = if 1+ else drop then  \ Increment T counter
        drop  \ Remove the character from stack
    loop
    2drop  \ Remove string address and length
    . . . .  \ Print the four counts
;

\ Most efficient implementation
: count-nucleotides-efficient ( addr len -- )
    0 0 0 0  \ A C G T counters
    0 do
        over i + c@  \ Get character
        case
            [char] A of 1+ 0 0 0 endof
            [char] C of 0 1+ 0 0 endof
            [char] G of 0 0 1+ 0 endof
            [char] T of 0 0 0 1+ endof
            0 0 0 0  \ Default case (ignore non-nucleotide chars)
        endcase
    loop
    2drop  \ Remove string address and length
    . . . .  \ Print results
;

\ Example usage:
\ : test-string s" AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC" ;
\ test-string count-nucleotides
```

## Example Usage

```forth
\ Input: AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
\ Expected output: 20 12 17 21

\ Define the DNA string
s" AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC" count-nucleotides
```

## Explanation

The solution works by:

1. **Initializing counters**: Four variables are initialized to zero to count A, C, G, and T nucleotides
2. **Iterating through the string**: A loop processes each character in the DNA string
3. **Character matching**: Each character is compared against A, C, G, and T using the `case` statement
4. **Incrementing counters**: When a match is found, the corresponding counter is incremented
5. **Output**: The final counts are printed in the order A C G T

## Key Forth Concepts Used

- **String handling**: Using address and length to access string characters
- **Looping**: `0 do ... loop` for iterating through string characters
- **Conditional execution**: `case ... endcase` for character matching
- **Stack manipulation**: Proper handling of stack values during operations
- **Memory management**: Using `2drop` to clean up stack after processing

The solution efficiently processes the DNA string in O(n) time where n is the length of the string.

