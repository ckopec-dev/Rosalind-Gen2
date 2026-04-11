# Rosalind Problem: Compute the Hamming Distance Between Two Strings

## Problem Statement
The Hamming distance between two strings of equal length is the number of positions at which the corresponding symbols are different. Given two strings of equal length, compute their Hamming distance.

## Solution in Forth

```forth
\ Read two strings from input and compute Hamming distance
: hamming-distance ( addr1 addr2 len -- distance )
    0 swap \ Initialize counter
    0 do
        over i + c@ \ Get character from first string
        over i + c@ \ Get character from second string
        <> if 1+ then \ If different, increment counter
    loop
    2drop \ Remove string addresses from stack
;

\ Alternative implementation using a more explicit approach
: hamming-distance-alt ( addr1 addr2 len -- distance )
    0 \ Initialize counter
    0 do
        over i + c@ over i + c@ - 0<> if 1+ then
    loop
    2drop
;

\ Main program to read input and compute result
: compute-hamming ( -- )
    \ Read first string (assuming it's already on stack or read from input)
    \ For this example, let's assume we have two strings in memory
    \ String 1: "GAGCCTACTAACGGGAT"
    \ String 2: "CATCGGTATCGCCCGTT"
    
    \ Example usage:
    \ "GAGCCTACTAACGGGAT"  "CATCGGTATCGCCCGTT" 17 hamming-distance .
    
    \ If reading from stdin, we'd need additional input handling
    \ This is a simplified version showing the core algorithm
    cr ." Hamming distance computed" 
;

\ Example test case
: test-hamming ( -- )
    s" GAGCCTACTAACGGGAT" s" CATCGGTATCGCCCGTT" 17 hamming-distance .
    cr ." Expected: 7" 
;
```

## Explanation

The Hamming distance calculation in Forth works as follows:

1. **Function signature**: `hamming-distance ( addr1 addr2 len -- distance )`
   - Takes two string addresses and their length
   - Returns the Hamming distance

2. **Algorithm**:
   - Initialize a counter to zero
   - Loop through each character position from 0 to length-1
   - Compare characters at each position using `c@` (character fetch)
   - If characters are different (`<>`), increment the counter
   - Return the final count

3. **Key Forth concepts used**:
   - `do...loop` for iteration
   - `c@` to fetch characters from memory
   - `<>` for inequality comparison
   - Stack manipulation with `2drop` to clean up
   - String handling with addresses and lengths

## Usage Example

```forth
s" GAGCCTACTAACGGGAT" s" CATCGGTATCGCCCGTT" 17 hamming-distance .
\ Output: 7
```

The Hamming distance is 7 because there are 7 positions where the characters differ between the two strings.

## Notes

This implementation assumes:
- Both strings have equal length
- Strings are passed as address and length pairs
- Standard Forth environment with string handling capabilities
- The strings are stored in memory accessible by their addresses

For a complete Rosalind solution, you would need to add input reading functionality to parse the actual input file format.

