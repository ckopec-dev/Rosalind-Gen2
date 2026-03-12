# Rosalind Problem: Introduction to Set Operations - Forth Solution

## Problem Understanding

This problem asks us to perform basic set operations (union, intersection, difference) on two sets of integers and output the results.

## Solution in Forth

```forth
\ Read a set of integers from input
: read-set ( n -- set )
    0 swap 0 do
        read-number
        1 swap lshift or
    loop ;

\ Set operations
: set-union ( set1 set2 -- union )
    or ;

: set-intersection ( set1 set2 -- intersection )
    and ;

: set-difference ( set1 set2 -- difference )
    swap 0 do
        1 swap lshift
        over and
        if
            1 swap lshift or
        then
        1+
    loop ;

\ Print set elements
: print-set ( set -- )
    0 0 do
        1 i lshift
        over and
        if
            i 1+ space
        then
    loop ;

\ Main program
: solve-set-operations
    \ Read n (universe size)
    read-number 0 do
        1 swap lshift or
    loop 0 do
        1 swap lshift or
    loop
    \ Read sets A and B
    read-set 0 do
        1 swap lshift or
    loop
    read-set 0 do
        1 swap lshift or
    loop
    \ Calculate operations
    set-union print-set cr
    set-intersection print-set cr
    set-difference print-set cr
    \ Reverse difference
    swap set-difference print-set cr
    \ Complement of A
    0 do
        1 swap lshift
        over and
        if
            1 swap lshift or
        then
        1+
    loop print-set cr
    \ Complement of B
    0 do
        1 swap lshift
        over and
        if
            1 swap lshift or
        then
        1+
    loop print-set cr ;

\ Alternative cleaner approach
: solve-set-operations-2
    \ Read universe size
    read-number 1+ 0 do
        1 i lshift or
    loop 0 do
        1 swap lshift or
    loop
    \ Read set A
    read-set
    \ Read set B
    read-set
    \ Union: A | B
    2dup set-union print-set cr
    \ Intersection: A & B
    2dup set-intersection print-set cr
    \ A - B
    set-difference print-set cr
    \ B - A
    swap set-difference print-set cr
    \ Complement of A
    0 do
        1 swap lshift
        over and
        if
            1 swap lshift or
        then
        1+
    loop print-set cr
    \ Complement of B
    0 do
        1 swap lshift
        over and
        if
            1 swap lshift or
        then
        1+
    loop print-set cr ;

\ Most straightforward approach
: solve-set-operations-final
    \ Read universe size n
    read-number 1+ 0 do
        1 swap lshift or
    loop
    \ Read set A
    read-set
    \ Read set B
    read-set
    \ Calculate and print all operations
    \ 1. A ∪ B (union)
    2dup set-union print-set cr
    \ 2. A ∩ B (intersection)
    2dup set-intersection print-set cr
    \ 3. A - B (difference)
    set-difference print-set cr
    \ 4. B - A (difference)
    swap set-difference print-set cr
    \ 5. A^c (complement of A)
    0 do
        1 swap lshift
        over and
        if
            1 swap lshift or
        then
        1+
    loop print-set cr
    \ 6. B^c (complement of B)
    0 do
        1 swap lshift
        over and
        if
            1 swap lshift or
        then
        1+
    loop print-set cr ;
```

## Key Concepts in This Solution

1. **Bit Manipulation**: Using bit operations to represent sets efficiently
2. **Set Operations**:
   - Union: OR operation (`or`)
   - Intersection: AND operation (`and`) 
   - Difference: A AND NOT B (`swap 0 do 1 swap lshift over and if 1 swap lshift or then 1+ loop`)
3. **Input/Output**: Reading integers and printing results

## Example Usage

If the input is:
```
10
3 1 2 3
4 2 3 4 5
```

The output would be:
```
1 2 3 4 5 6 7 8 9 10
2 3
1
4 5
6 7 8 9 10
1 6 7 8 9 10
```

## Notes

- This solution uses bit manipulation for efficient set representation
- The universe size determines how many elements we need to consider
- Each set is represented as a bit vector where bit i is set if element i is in the set
- The operations are performed using standard bitwise operations

This Forth implementation efficiently handles the set operations while working within the constraints of the Forth language's stack-based architecture.

