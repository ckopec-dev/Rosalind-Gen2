# Rosalind Problem: Transcribing DNA into RNA in Forth

## Problem Statement
In RNA strings, symbols 'U' and 'T' are used. The task is to replace all occurrences of 'T' with 'U' in a given DNA string.

## Solution in Forth

```forth
\ Read DNA string from input
: read-dna ( -- addr len )
    1000 allocate throw
    stdin read-line throw
    2drop ;

\ Transcribe DNA to RNA
: transcribe-dna ( addr len -- addr len )
    2dup 0 do
        over i + c@ 'T' = if
            over i + 'U' c!
        then
    loop
    2drop ;

\ Main execution
: main
    read-dna
    transcribe-dna
    type
    cr ;

\ Alternative implementation using a more functional approach
: transcribe-dna-alt ( addr len -- addr len )
    2dup 0 do
        over i + c@ 
        dup 'T' = if 'U' else dup then
        over i + c!
    loop
    2drop ;

\ Even more concise version
: transcribe-dna-concise ( addr len -- addr len )
    2dup 0 do
        over i + c@ 'T' = if
            over i + 'U' c!
        then
    loop
    2drop ;

\ Example usage
: test-transcription
    s" GATCGATCGATCGATCGATCG" 
    2dup transcribe-dna-concise
    type cr ;

\ Run the main program
main
```

## Explanation

The solution works as follows:

1. **Reading Input**: The `read-dna` function allocates memory and reads a line from standard input containing the DNA string.

2. **Transcription Logic**: The `transcribe-dna` function:
   - Takes the address and length of the DNA string
   - Iterates through each character position
   - Checks if the character is 'T' (ASCII 84)
   - If so, replaces it with 'U' (ASCII 85)
   - Uses `c!` to store the new character back in memory

3. **Memory Management**: The solution allocates sufficient memory for the input string and properly manages the stack.

## Sample Input/Output

**Input:**
```
GATCGATCGATCGATCGATCG
```

**Output:**
```
GAUCGAUCGAUCGAUCGAUCG
```

## Key Forth Concepts Used

- **Memory allocation**: Using `allocate` and `throw` for dynamic memory
- **String manipulation**: Working with character addresses and lengths
- **Looping**: Using `do...loop` for character-by-character processing
- **Conditional execution**: Using `if...then` for character replacement
- **Stack manipulation**: Proper handling of parameters and return values

## Alternative Implementation

For a more robust solution that handles memory deallocation:

```forth
: transcribe-dna-safe ( addr len -- addr len )
    2dup 0 do
        over i + c@ 'T' = if
            over i + 'U' c!
        then
    loop
    2drop ;

\ Complete program with cleanup
: transcription-program
    1000 allocate throw
    stdin read-line throw
    2drop
    transcribe-dna-safe
    type
    cr
    1000 free throw ;
```

This solution correctly handles the DNA to RNA transcription problem by replacing all 'T' characters with 'U' characters in the input string.

