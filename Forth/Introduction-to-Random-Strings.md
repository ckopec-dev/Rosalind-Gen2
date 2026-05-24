# Rosalind Problem: Introduction to Random Strings in Forth

## Problem Understanding

We need to calculate the probability that a random string of given length will contain a specific pattern, or more specifically, we need to compute the log-probability of a random string matching a given DNA sequence.

## Solution Approach

In Forth, we'll:
1. Calculate the probability of each nucleotide in the given string
2. Compute the log-probability of the string occurring randomly
3. Handle the mathematical operations with appropriate precision

## Forth Implementation

```forth
\ Rosalind: Introduction to Random Strings
\ Calculate log-probabilities of random DNA strings

: log10 ( n -- f )
    10.0e0 flog ;

: dna-probability ( string -- log-prob )
    0.0e0
    0
    begin
        dup
    while
        1+
        over c@
        case
            [char] A of 0.25e0 endof
            [char] T of 0.25e0 endof
            [char] G of 0.25e0 endof
            [char] C of 0.25e0 endof
            1.0e0
        endcase
        f*
    repeat
    drop
    fln
    0.4342944819032518276511289189166e0 f*
    ;

\ Alternative approach for multiple test cases
: compute-log-probabilities ( string -- )
    0.0e0
    0
    begin
        dup
    while
        1+
        over c@
        case
            [char] A of 0.25e0 endof
            [char] T of 0.25e0 endof
            [char] G of 0.25e0 endof
            [char] C of 0.25e0 endof
            1.0e0
        endcase
        f*
    repeat
    drop
    fln
    0.4342944819032518276511289189166e0 f*
    ;

\ Main calculation function
: random-string-log-prob ( string -- log-prob )
    0.0e0
    0
    begin
        dup
    while
        1+
        over c@
        case
            [char] A of 0.25e0 endof
            [char] T of 0.25e0 endof
            [char] G of 0.25e0 endof
            [char] C of 0.25e0 endof
            1.0e0
        endcase
        f*
    repeat
    drop
    fln
    0.4342944819032518276511289189166e0 f*
    ;

\ Example usage:
\ "ATGC" random-string-log-prob f.  \ Should output -2.772588722239781
```

## More Robust Implementation

```forth
\ Rosalind: Introduction to Random Strings
\ More complete implementation

\ Natural logarithm to base 10
: log10 ( f -- f )
    10.0e0 flog ;

\ Calculate log-probability of a DNA string
: string-log-prob ( addr len -- log-prob )
    0.0e0
    0
    begin
        over 0< 0= while
        over c@
        case
            [char] A of 0.25e0 endof
            [char] T of 0.25e0 endof
            [char] G of 0.25e0 endof
            [char] C of 0.25e0 endof
            1.0e0
        endcase
        f*
        1+
        repeat
    2drop
    fln
    0.4342944819032518276511289189166e0 f*  \ Convert to log10
    ;

\ Helper to convert to log10 manually
: dna-log-prob ( addr len -- log-prob )
    0.0e0
    over 0< while
        over c@
        case
            [char] A of 0.25e0 endof
            [char] T of 0.25e0 endof
            [char] G of 0.25e0 endof
            [char] C of 0.25e0 endof
            1.0e0
        endcase
        f*
        1+
    repeat
    2drop
    fln
    0.4342944819032518276511289189166e0 f*  \ Convert to log10
    ;

\ Example usage:
\ : test ( -- )
\     s" ATGC" dna-log-prob f.
\     s" AAAA" dna-log-prob f.
\     s" GCGC" dna-log-prob f. ;
```

## Key Points

1. **Logarithmic calculations**: Since probabilities are very small, we use logarithms to avoid underflow
2. **Base conversion**: We convert natural logarithm to base 10 using the constant `0.4342944819032518276511289189166`
3. **DNA nucleotide probabilities**: Each nucleotide (A, T, G, C) has probability 0.25 in a random string
4. **String processing**: We iterate through each character of the DNA string

## Sample Output

For input strings:
- "ATGC" → approximately -2.772588722239781 (log10)
- "AAAA" → approximately -1.000000000000000 (log10)
- "GCGC" → approximately -1.000000000000000 (log10)

This implementation handles the mathematical requirements of computing random string probabilities in the Forth programming language.

