# Rosalind Problem: Identifying Reversing Substitutions in Fortran

## Problem Understanding

The problem asks us to identify reversing substitutions in DNA sequences. A reversing substitution occurs when a nucleotide changes to its complement (A↔T, C↔G) but the overall sequence remains functionally equivalent in terms of base pairing.

## Solution Approach

Let me implement a Fortran solution to identify reversing substitutions:

```fortran
program identifying_reversing_substitutions
    implicit none
    character(len=1), parameter :: A = 'A', T = 'T', C = 'C', G = 'G'
    character(len=1) :: nucleotide
    integer :: i, j, n, count
    character(len=1000) :: sequence1, sequence2
    logical :: is_reversing_substitution
    
    ! Read input sequences
    read(*,*) sequence1
    read(*,*) sequence2
    
    ! Get length of sequences
    n = len_trim(sequence1)
    
    ! Initialize counter
    count = 0
    
    ! Check each position for reversing substitutions
    do i = 1, n
        if (sequence1(i:i) /= sequence2(i:i)) then
            ! Check if it's a reversing substitution
            if (is_reversing(sequence1(i:i), sequence2(i:i))) then
                count = count + 1
                write(*,*) i, sequence1(i:i), sequence2(i:i)
            end if
        end if
    end do
    
    write(*,*) "Total reversing substitutions:", count
    
contains
    
    logical function is_reversing(nuc1, nuc2)
        character(len=1), intent(in) :: nuc1, nuc2
        
        is_reversing = .false.
        
        select case (nuc1)
            case ('A')
                if (nuc2 == 'T') then
                    is_reversing = .true.
                end if
            case ('T')
                if (nuc2 == 'A') then
                    is_reversing = .true.
                end if
            case ('C')
                if (nuc2 == 'G') then
                    is_reversing = .true.
                end if
            case ('G')
                if (nuc2 == 'C') then
                    is_reversing = .true.
                end if
        end select
    end function is_reversing
    
end program identifying_reversing_substitutions
```

## Alternative Implementation with More Detailed Analysis

```fortran
program reversing_substitution_analysis
    implicit none
    character(len=1) :: nucleotide1, nucleotide2
    integer :: i, j, n, total_count, position
    character(len=1000) :: seq1, seq2
    logical :: is_complement
    
    ! Read two DNA sequences
    read(*,*) seq1
    read(*,*) seq2
    
    ! Get sequence length
    n = len_trim(seq1)
    
    ! Initialize counters
    total_count = 0
    
    ! Analyze each position
    do i = 1, n
        if (seq1(i:i) /= seq2(i:i)) then
            ! Check if this is a reversing substitution
            if (is_complement_pair(seq1(i:i), seq2(i:i))) then
                total_count = total_count + 1
                write(*,*) 'Position ', i, ': ', seq1(i:i), ' -> ', seq2(i:i)
            end if
        end if
    end do
    
    write(*,*) 'Number of reversing substitutions found:', total_count
    
contains
    
    logical function is_complement_pair(nuc1, nuc2)
        character(len=1), intent(in) :: nuc1, nuc2
        
        is_complement_pair = .false.
        
        select case (nuc1)
            case ('A')
                if (nuc2 == 'T') then
                    is_complement_pair = .true.
                end if
            case ('T')
                if (nuc2 == 'A') then
                    is_complement_pair = .true.
                end if
            case ('C')
                if (nuc2 == 'G') then
                    is_complement_pair = .true.
                end if
            case ('G')
                if (nuc2 == 'C') then
                    is_complement_pair = .true.
                end if
        end select
    end function is_complement_pair
    
end program reversing_substitution_analysis
```

## Sample Input/Output

**Input:**
```
ATCGATCG
TAGCTAGC
```

**Expected Output:**
```
Position 1: A -> T
Position 2: T -> A
Position 3: C -> G
Position 4: G -> C
Position 5: A -> T
Position 6: T -> A
Position 7: C -> G
Position 8: G -> C
Number of reversing substitutions found: 8
```

## Key Features of the Solution

1. **Input Reading**: Reads two DNA sequences from standard input
2. **Position-by-Position Comparison**: Compares each nucleotide position
3. **Complement Check**: Identifies if nucleotides form complementary pairs (A↔T, C↔G)
4. **Output**: Reports positions where reversing substitutions occur
5. **Counting**: Keeps track of total number of reversing substitutions

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the sequences
- **Space Complexity**: O(n) for storing the sequences

This solution correctly identifies reversing substitutions by checking if each mismatched position contains complementary nucleotides that would maintain base pairing functionality.

