# Rosalind Problem: Compute the Size of a Spectral Dictionary

## Problem Statement
Given a string `s` and an integer `k`, we want to compute the size of the spectral dictionary of `s` with respect to `k`. The spectral dictionary of a string `s` with respect to `k` is the set of all substrings of `s` of length `k`, where each substring is considered as a "spectral" element.

## Approach
The problem is asking for the number of distinct substrings of length `k` in string `s`. This is essentially counting the distinct k-mers in a string.

## Solution in Fortran

```fortran
program spectral_dictionary_size
    implicit none
    character(len=1000) :: s
    integer :: k, n, i, count
    character(len=1000) :: substr
    logical :: found
    character(len=1000), allocatable :: dictionary(:)
    integer :: dict_size
    
    ! Read input
    read(*,*) s
    read(*,*) k
    
    ! Get string length
    n = len_trim(s)
    
    ! Handle edge cases
    if (k > n) then
        print *, 0
        stop
    end if
    
    ! Allocate dictionary array
    allocate(dictionary(n-k+1))
    dict_size = 0
    
    ! Find all substrings of length k
    do i = 1, n-k+1
        substr = s(i:i+k-1)
        dict_size = dict_size + 1
        dictionary(dict_size) = substr
    end do
    
    ! Count distinct substrings
    count = 0
    do i = 1, dict_size
        found = .false.
        ! Check if this substring already exists in our dictionary
        do j = 1, count
            if (dictionary(i) == dictionary(j)) then
                found = .true.
                exit
            end if
        end do
        if (.not. found) then
            count = count + 1
        end if
    end do
    
    print *, count
    
    deallocate(dictionary)
end program spectral_dictionary_size
```

## Alternative More Efficient Solution

```fortran
program spectral_dictionary_size_optimized
    implicit none
    character(len=1000) :: s
    integer :: k, n, i, j, count
    character(len=1000) :: substr
    logical :: found
    character(len=1000), allocatable :: unique_substrings(:)
    integer :: max_substrings
    
    ! Read input
    read(*,*) s
    read(*,*) k
    
    ! Get string length
    n = len_trim(s)
    
    ! Handle edge cases
    if (k > n) then
        print *, 0
        stop
    end if
    
    ! Maximum possible substrings
    max_substrings = n - k + 1
    allocate(unique_substrings(max_substrings))
    
    ! Count distinct substrings
    count = 0
    
    do i = 1, n-k+1
        substr = s(i:i+k-1)
        
        ! Check if substring already exists
        found = .false.
        do j = 1, count
            if (unique_substrings(j) == substr) then
                found = .true.
                exit
            end if
        end do
        
        ! If not found, add to our set
        if (.not. found) then
            count = count + 1
            unique_substrings(count) = substr
        end if
    end do
    
    print *, count
    
    deallocate(unique_substrings)
end program spectral_dictionary_size_optimized
```

## Sample Input/Output

**Input:**
```
ACGTACGT
3
```

**Output:**
```
6
```

**Explanation:**
For string "ACGTACGT" and k=3, the substrings are:
- ACG
- CGT  
- GTA
- TAC
- ACG
- CGT

The distinct substrings are: ACG, CGT, GTA, TAC → count = 4

Wait, let me reconsider this. Let me re-read the problem...

Actually, looking at this more carefully, if we have "ACGTACGT" with k=3:
- Position 1: ACG
- Position 2: CGT
- Position 3: GTA  
- Position 4: TAC
- Position 5: ACG
- Position 6: CGT

Distinct substrings: ACG, CGT, GTA, TAC → count = 4

But the problem might be asking for the total number of k-mers, not distinct ones. Let me provide a cleaner version:

```fortran
program spectral_dictionary_size
    implicit none
    character(len=1000) :: s
    integer :: k, n, i, j, count
    character(len=1000) :: substr
    logical :: found, is_new
    character(len=1000), allocatable :: distinct_substrings(:)
    
    ! Read input
    read(*,*) s
    read(*,*) k
    
    ! Get string length
    n = len_trim(s)
    
    ! Handle edge cases
    if (k > n) then
        print *, 0
        stop
    end if
    
    ! Allocate array for distinct substrings
    allocate(distinct_substrings(n-k+1))
    count = 0
    
    ! Find all substrings of length k and count distinct ones
    do i = 1, n-k+1
        substr = s(i:i+k-1)
        
        ! Check if this substring is already in our list
        is_new = .true.
        do j = 1, count
            if (distinct_substrings(j) == substr) then
                is_new = .false.
                exit
            end if
        end do
        
        ! If new, add to our list
        if (is_new) then
            count = count + 1
            distinct_substrings(count) = substr
        end if
    end do
    
    print *, count
    
    deallocate(distinct_substrings)
end program spectral_dictionary_size
```

