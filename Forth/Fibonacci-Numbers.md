# Rosalind Problem: Fibonacci Numbers (Forth Solution)

## Problem Statement
Given: A positive integer n ≤ 1000.

Return: The total number of rabbit pairs that will be present after n months, if we begin with 1 pair and in each generation, every pair breeds a pair of offspring.

This is the classic Fibonacci sequence problem where:
- F(1) = 1
- F(2) = 1  
- F(n) = F(n-1) + F(n-2) for n > 2

## Forth Solution

```forth
\ Read input n
: get-n ( -- n )
    stdin read-line nip number> ;

\ Calculate fibonacci number
: fib ( n -- fib_n )
    dup 1 <= if
        drop 1
    else
        2dup - 1- fib >r
        2dup - 2- fib r>
        +
    then ;

\ Main execution
: main
    get-n fib . ;

main
```

## Alternative Iterative Solution (More Efficient)

```forth
\ Read input n
: get-n ( -- n )
    stdin read-line nip number> ;

\ Calculate fibonacci number iteratively
: fib-iter ( n -- fib_n )
    dup 1 <= if
        drop 1
    else
        2dup 2- 0 1
        do
            2dup + swap
        loop
        nip nip
    then ;

\ Main execution
: main
    get-n fib-iter . ;

main
```

## Explanation

### First Solution (Recursive):
- Uses a recursive approach to calculate Fibonacci numbers
- Base cases: F(1) = 1, F(2) = 1
- Recursive case: F(n) = F(n-1) + F(n-2)
- Uses `>r` and `r>` for stack management

### Second Solution (Iterative):
- More efficient approach using a loop
- Maintains two variables representing consecutive Fibonacci numbers
- Updates them iteratively until reaching the desired position
- Avoids potential stack overflow issues with deep recursion

### Key Forth Concepts Used:
- `stdin read-line` - reads input from standard input
- `number>` - converts string to number
- `dup` - duplicates top of stack
- `if ... else ... then` - conditional execution
- `do ... loop` - loop construct
- `nip` - removes second item from stack
- `swap` - exchanges top two stack items

### Sample Input/Output:
```
Input: 5
Output: 5

Input: 10  
Output: 55
```

The iterative solution is recommended for larger values of n as it's more efficient and avoids potential stack overflow issues.

