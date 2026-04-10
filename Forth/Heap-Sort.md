# Rosalind Problem: Heap Sort in Forth

## Problem Understanding

Heap sort is a comparison-based sorting algorithm that uses a binary heap data structure. The algorithm works by:
1. Building a max heap from the input array
2. Repeatedly extracting the maximum element and rebuilding the heap

## Forth Implementation

```forth
\ Heap Sort implementation in Forth

\ Array operations
: array-size ( addr -- n )  @ ;
: array-set-size ( n addr -- )  ! ;
: array-ref ( addr i -- addr )  cells + ;
: array-set ( val addr i -- )  array-ref ! ;

\ Heap operations
: parent ( i -- parent )  2 / ;
: left-child ( i -- left )  2 * 1 + ;
: right-child ( i -- right )  2 * 2 + ;

\ Heapify operation - maintains max heap property
: heapify ( addr heap-size i -- )
    >r
    r@ left-child
    r@ right-child
    2dup > if 2drop else swap then  \ Get larger child
    r@ array-ref  r@ array-ref  > if  \ If child > parent
        r@ array-ref  r@ array-ref  r@ array-set  \ Swap
        r@ array-set-size  \ Update heap size
        r> heapify  \ Recursively heapify
    else
        r> drop
    then ;

\ Build max heap
: build-heap ( addr heap-size -- )
    2dup 2 / 1+ 0 do
        2dup i heapify
    loop
    2drop ;

\ Heap sort main function
: heap-sort ( addr n -- )
    dup 1+ 0 do
        \ Build max heap for remaining elements
        2dup i build-heap
        \ Swap root with last element
        0 i array-ref  i array-ref  swap  i array-set
        \ Reduce heap size
        1- 0 do
            0 i heapify
        loop
    loop
    2drop ;

\ Example usage
: test-heap-sort ( -- )
    10 2* cells allocate throw  \ Allocate array for 10 integers
    dup 50 swap array-set
    dup 10 swap array-set
    dup 20 swap array-set
    dup 30 swap array-set
    dup 40 swap array-set
    dup 60 swap array-set
    dup 70 swap array-set
    dup 80 swap array-set
    dup 90 swap array-set
    dup 100 swap array-set
    10 heap-sort
    \ Print sorted array
    10 0 do
        i array-ref . space
    loop
    cr
    free throw ;

\ Alternative cleaner implementation
: heapify-down ( addr heap-size i -- )
    >r
    begin
        r@ left-child
        r@ right-child
        2dup > if 2drop else swap then  \ Get larger child
        r@ array-ref  r@ array-ref  > if  \ If child > parent
            r@ array-ref  r@ array-ref  r@ array-set  \ Swap
            r@ array-set-size  \ Update heap size
            r> heapify-down  \ Recursively heapify
        else
            r> drop
            false
        then
    until ;

: heap-sort-clean ( addr n -- )
    dup 1+ 0 do
        \ Build max heap
        2dup i build-heap
        \ Swap root with last element
        0 i array-ref  i array-ref  swap  i array-set
        \ Reduce heap size and heapify
        1- 0 do
            0 i heapify-down
        loop
    loop
    2drop ;
```

## Key Features of the Implementation

1. **Array Operations**: Basic array access and modification functions
2. **Heap Operations**: Parent, left child, right child calculations
3. **Heapify Function**: Maintains max heap property by comparing with children
4. **Build Heap**: Converts array into max heap structure
5. **Heap Sort**: Main sorting algorithm combining heap operations

## Algorithm Steps

1. **Build Max Heap**: Convert input array into max heap structure
2. **Extract Maximum**: Move root (maximum element) to end of array
3. **Rebuild Heap**: Restore heap property for remaining elements
4. **Repeat**: Continue until all elements are sorted

## Time and Space Complexity

- **Time Complexity**: O(n log n)
- **Space Complexity**: O(1) - in-place sorting

## Usage Example

```forth
\ Sort an array of 10 integers
10 2* cells allocate throw  \ Allocate space
\ Fill array with values
\ ... set values ...
10 heap-sort  \ Sort the array
```

This implementation follows Forth's stack-based approach and uses standard heap sort algorithms adapted for Forth's memory model and syntax.

