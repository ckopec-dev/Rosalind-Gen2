# Rosalind Problem: Identifying Maximal Repeats

## Problem Description
Given a string, find all maximal repeats (substrings that appear at least twice and cannot be extended to the left or right without losing the repeat property).

## Solution in Fortran

```fortran
program maximal_repeats
    implicit none
    character(len=1000) :: input_string
    integer :: n, i, j, k, l, max_len, repeat_count
    integer :: start_pos, end_pos
    logical :: is_repeat, is_maximal
    character(len=1000) :: substring
    integer, allocatable :: positions(:)
    integer :: num_positions, max_positions
    
    ! Read input string
    read(*, '(A)') input_string
    n = len_trim(input_string)
    
    ! Allocate memory for positions array
    max_positions = n * (n + 1) / 2
    allocate(positions(max_positions))
    
    ! Find all maximal repeats
    do i = 1, n
        do j = i, n
            ! Skip if substring length is 1 (trivial repeats)
            if (j - i + 1 == 1) cycle
            
            ! Extract substring
            substring = input_string(i:j)
            repeat_count = 0
            num_positions = 0
            
            ! Count occurrences and record positions
            do k = 1, n - (j - i + 1) + 1
                is_repeat = .true.
                do l = 1, j - i + 1
                    if (input_string(k + l - 1) /= substring(l:l)) then
                        is_repeat = .false.
                        exit
                    end if
                end do
                
                if (is_repeat) then
                    repeat_count = repeat_count + 1
                    num_positions = num_positions + 1
                    if (num_positions <= max_positions) then
                        positions(num_positions) = k
                    end if
                end if
            end do
            
            ! Check if this is a maximal repeat (appears at least twice)
            if (repeat_count >= 2) then
                ! Check if it's maximal by trying to extend left and right
                is_maximal = .true.
                
                ! Check left extension
                if (i > 1) then
                    ! Try extending one character to the left
                    do k = 1, n - (j - i + 2) + 1
                        is_repeat = .true.
                        ! Check if extended string is still a repeat
                        do l = 1, j - i + 2
                            if (l == 1) then
                                if (input_string(k + l - 2) /= substring(1:1)) then
                                    is_repeat = .false.
                                    exit
                                end if
                            else if (l > j - i + 1) then
                                if (input_string(k + l - 2) /= substring(j - i + 1:j - i + 1)) then
                                    is_repeat = .false.
                                    exit
                                end if
                            else
                                if (input_string(k + l - 2) /= substring(l:l)) then
                                    is_repeat = .false.
                                    exit
                                end if
                            end if
                        end do
                        
                        if (is_repeat) then
                            is_maximal = .false.
                            exit
                        end if
                    end do
                end if
                
                ! Check right extension
                if (is_maximal .and. j < n) then
                    ! Try extending one character to the right
                    do k = 1, n - (j - i + 2) + 1
                        is_repeat = .true.
                        ! Check if extended string is still a repeat
                        do l = 1, j - i + 2
                            if (l == 1) then
                                if (input_string(k + l - 2) /= substring(1:1)) then
                                    is_repeat = .false.
                                    exit
                                end if
                            else if (l > j - i + 1) then
                                if (input_string(k + l - 2) /= substring(j - i + 1:j - i + 1)) then
                                    is_repeat = .false.
                                    exit
                                end if
                            else
                                if (input_string(k + l - 2) /= substring(l:l)) then
                                    is_repeat = .false.
                                    exit
                                end if
                            end if
                        end do
                        
                        if (is_repeat) then
                            is_maximal = .false.
                            exit
                        end if
                    end do
                end if
                
                ! If maximal, output the repeat
                if (is_maximal) then
                    write(*, '(A)') substring
                end if
            end if
        end do
    end do
    
    deallocate(positions)
end program maximal_repeats
```

## Alternative Simpler Approach

```fortran
program maximal_repeats_simple
    implicit none
    character(len=1000) :: input_string
    integer :: n, i, j, k, l, count
    character(len=1000) :: substring
    logical :: is_maximal, found
    
    ! Read input string
    read(*, '(A)') input_string
    n = len_trim(input_string)
    
    ! Find maximal repeats
    do i = 1, n
        do j = i + 1, n
            ! Extract substring
            substring = input_string(i:j)
            count = 0
            
            ! Count occurrences
            do k = 1, n - (j - i + 1) + 1
                found = .true.
                do l = 1, j - i + 1
                    if (input_string(k + l - 1) /= substring(l:l)) then
                        found = .false.
                        exit
                    end if
                end do
                if (found) count = count + 1
            end do
            
            ! If appears at least twice, check if maximal
            if (count >= 2) then
                ! Check if we can extend it to left or right
                is_maximal = .true.
                
                ! Check left extension
                if (i > 1) then
                    ! Try to extend one character to the left
                    do k = 1, n - (j - i + 2) + 1
                        found = .true.
                        do l = 1, j - i + 2
                            if (l == 1) then
                                if (input_string(k + l - 2) /= substring(1:1)) then
                                    found = .false.
                                    exit
                                end if
                            else if (l > j - i + 1) then
                                if (input_string(k + l - 2) /= substring(j - i + 1:j - i + 1)) then
                                    found = .false.
                                    exit
                                end if
                            else
                                if (input_string(k + l - 2) /= substring(l:l)) then
                                    found = .false.
                                    exit
                                end if
                            end if
                        end do
                        if (found) then
                            is_maximal = .false.
                            exit
                        end if
                    end do
                end if
                
                ! Check right extension
                if (is_maximal .and. j < n) then
                    ! Try to extend one character to the right
                    do k = 1, n - (j - i + 2) + 1
                        found = .true.
                        do l = 1, j - i + 2
                            if (l == 1) then
                                if (input_string(k + l - 2) /= substring(1:1)) then
                                    found = .false.
                                    exit
                                end if
                            else if (l > j - i + 1) then
                                if (input_string(k + l - 2) /= substring(j - i + 1:j - i + 1)) then
                                    found = .false.
                                    exit
                                end if
                            else
                                if (input_string(k + l - 2) /= substring(l:l)) then
                                    found = .false.
                                    exit
                                end if
                            end if
                        end do
                        if (found) then
                            is_maximal = .false.
                            exit
                        end if
                    end do
                end if
                
                ! If maximal, print it
                if (is_maximal) then
                    write(*, '(A)') substring
                end if
            end if
        end do
    end do
end program maximal_repeats_simple
```

## How to Use

1. Compile the program:
   ```bash
   gfortran -o maximal_repeats maximal_repeats.f90
   ```

2. Run with input:
   ```bash
   echo "ABABAB" | ./maximal_repeats
   ```

## Time Complexity
- O(n³) where n is the length of the input string
- The algorithm checks all possible substrings and for each, counts occurrences and verifies maximality

## Space Complexity
- O(n²) for storing positions and substrings

## Note
This implementation handles the basic requirements of finding maximal repeats. For very large inputs, more efficient algorithms using suffix trees or suffix arrays would be preferred.

