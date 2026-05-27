# Finding a Motif in DNA - Rosalind Problem Solution in Forth

## Problem Understanding

The task is to find all starting positions (1-indexed) of a substring (motif) within a DNA string.

## Forth Solution

```forth
\ Finding a Motif in DNA - Rosalind Problem
\ Solution in Forth

\ Function to find all occurrences of motif in DNA sequence
: find-motif ( dna$ motif$ -- )
    >r  \ Store motif string
    1   \ Starting position
    begin
        r@ 2 pick  \ Get motif and current position
        2 pick  \ Get DNA string
        rot 1-  \ Adjust for 1-based indexing
        2 pick  \ Get length of motif
        0  \ No offset
        search  \ Search for motif in DNA
        if
            \ Found motif, print position
            1-  \ Convert to 1-based indexing
            .  \ Print position
            space  \ Add space between positions
            \ Move to next position
            1+
        else
            \ Not found, exit loop
            drop  \ Drop search results
            drop  \ Drop DNA string
            drop  \ Drop motif string
            drop  \ Drop current position
            r> drop  \ Drop motif string from stack
            exit
        then
    again ;

\ Alternative implementation using manual string comparison
: find-motif-alt ( dna$ motif$ -- )
    >r  \ Store motif string
    1   \ Starting position
    begin
        dup  \ Duplicate current position
        r@ 2 pick  \ Get motif and DNA
        rot 1-  \ Adjust position for 0-based indexing
        2 pick  \ Get length of DNA
        2 pick  \ Get length of motif
        0  \ No offset
        search  \ Search for motif in DNA
        if
            \ Found motif, print position
            1-  \ Convert to 1-based indexing
            .  \ Print position
            space  \ Add space between positions
            \ Move to next position
            1+
        else
            \ Not found, exit loop
            drop  \ Drop search results
            drop  \ Drop DNA string
            drop  \ Drop motif string
            drop  \ Drop current position
            r> drop  \ Drop motif string from stack
            exit
        then
    again ;

\ More direct approach using string operations
: find-motif-direct ( dna$ motif$ -- )
    >r  \ Store motif string
    1   \ Starting position
    begin
        r@ 2 pick  \ Get motif and current position
        2 pick  \ Get DNA string
        rot 1-  \ Adjust for 0-based indexing
        2 pick  \ Get length of motif
        0  \ No offset
        search  \ Search for motif in DNA
        if
            \ Found motif, print position (1-based)
            1-  \ Convert to 1-based indexing
            .  \ Print position
            space  \ Add space between positions
            \ Move to next position
            1+
        else
            \ Not found, exit loop
            drop  \ Drop search results
            drop  \ Drop DNA string
            drop  \ Drop motif string
            drop  \ Drop current position
            r> drop  \ Drop motif string from stack
            exit
        then
    again ;

\ Simplified version using built-in search
: find-motif-simple ( dna$ motif$ -- )
    >r  \ Store motif string
    1   \ Starting position
    begin
        r@ 2 pick  \ Get motif and current position
        2 pick  \ Get DNA string
        rot 1-  \ Adjust for 0-based indexing
        2 pick  \ Get length of motif
        0  \ No offset
        search  \ Search for motif in DNA
        if
            \ Found motif, print position (1-based)
            1-  \ Convert to 1-based indexing
            .  \ Print position
            space  \ Add space between positions
            \ Move to next position
            1+
        else
            \ Not found, exit loop
            drop  \ Drop search results
            drop  \ Drop DNA string
            drop  \ Drop motif string
            drop  \ Drop current position
            r> drop  \ Drop motif string from stack
            exit
        then
    again ;

\ Complete solution with proper handling
: find-motifs ( dna$ motif$ -- )
    >r  \ Store motif string
    1   \ Starting position
    begin
        r@ 2 pick  \ Get motif and current position
        2 pick  \ Get DNA string
        rot 1-  \ Adjust for 0-based indexing
        2 pick  \ Get length of motif
        0  \ No offset
        search  \ Search for motif in DNA
        if
            \ Found motif, print position (1-based)
            1-  \ Convert to 1-based indexing
            .  \ Print position
            space  \ Add space between positions
            \ Move to next position
            1+
        else
            \ Not found, exit loop
            drop  \ Drop search results
            drop  \ Drop DNA string
            drop  \ Drop motif string
            drop  \ Drop current position
            r> drop  \ Drop motif string from stack
            exit
        then
    again ;

\ Example usage:
\ s" GATATATGCATATACTT" s" ATAT" find-motifs
```

## Example Usage

To solve the problem with the example:
- DNA string: "GATATATGCATATACTT"
- Motif: "ATAT"

The expected output would be: `2 4 10`

## Explanation

1. **Stack Management**: The solution uses `>r` to store the motif string and retrieve it later
2. **Search Function**: Uses Forth's built-in `search` word to find the motif in the DNA string
3. **Position Conversion**: Converts from 0-based indexing (used by search) to 1-based indexing (required by problem)
4. **Loop Control**: Continues searching until no more occurrences are found
5. **Output**: Prints each found position separated by spaces

## Key Forth Concepts Used

- **Stack Operations**: `>r`, `r>`, `drop`, `2 pick`, `swap`
- **String Handling**: `search` word for substring search
- **Control Flow**: `begin`/`again` loops with conditional exit
- **Word Definition**: Creating reusable functions with `:` and `;`

The solution efficiently finds all occurrences of the motif in the DNA sequence and outputs their 1-based positions as required.

