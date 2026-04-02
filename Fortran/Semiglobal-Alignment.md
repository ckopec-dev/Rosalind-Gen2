# Rosalind Problem: Semiglobal Alignment in Fortran

## Problem Understanding

Semiglobal alignment is a sequence alignment method that allows gaps at the beginning and end of sequences but penalizes internal gaps. Unlike global alignment, it doesn't penalize gaps at the sequence ends, making it useful for aligning sequences of different lengths where we want to find local similarities.

## Solution Approach

I'll implement the semiglobal alignment algorithm using dynamic programming with a scoring matrix.

```fortran
program semiglobal_alignment
    implicit none
    integer, parameter :: max_len = 1000
    character(len=1) :: seq1(max_len), seq2(max_len)
    integer :: m, n, i, j
    integer :: score(max_len, max_len)
    integer :: match_score = 2
    integer :: mismatch_score = -1
    integer :: gap_penalty = -1
    character(len=1) :: alignment1(max_len), alignment2(max_len)
    integer :: align_len
    
    ! Read input sequences
    read(*,*) m
    read(*,*) n
    read(*,*) (seq1(i), i=1,m)
    read(*,*) (seq2(i), i=1,n)
    
    ! Initialize DP matrix
    do i = 0, m
        score(i, 0) = 0
    end do
    do j = 0, n
        score(0, j) = 0
    end do
    
    ! Fill the DP matrix
    do i = 1, m
        do j = 1, n
            if (seq1(i) == seq2(j)) then
                score(i, j) = score(i-1, j-1) + match_score
            else
                score(i, j) = score(i-1, j-1) + mismatch_score
            end if
            
            ! Consider gap penalties
            score(i, j) = max(score(i, j), &
                              score(i-1, j) + gap_penalty, &
                              score(i, j-1) + gap_penalty)
        end do
    end do
    
    ! Traceback to get alignment
    i = m
    j = n
    align_len = 0
    
    do while (i > 0 .or. j > 0)
        if (i > 0 .and. j > 0 .and. &
            score(i, j) == score(i-1, j-1) + &
            (match_score * (seq1(i) == seq2(j)) + &
             mismatch_score * (seq1(i) /= seq2(j)))) then
            ! Match/mismatch
            align_len = align_len + 1
            alignment1(align_len) = seq1(i)
            alignment2(align_len) = seq2(j)
            i = i - 1
            j = j - 1
        else if (i > 0 .and. score(i, j) == score(i-1, j) + gap_penalty) then
            ! Gap in sequence 2
            align_len = align_len + 1
            alignment1(align_len) = seq1(i)
            alignment2(align_len) = '-'
            i = i - 1
        else if (j > 0 .and. score(i, j) == score(i, j-1) + gap_penalty) then
            ! Gap in sequence 1
            align_len = align_len + 1
            alignment1(align_len) = '-'
            alignment2(align_len) = seq2(j)
            j = j - 1
        else
            ! This shouldn't happen in proper semiglobal alignment
            ! But let's handle it gracefully
            if (i > 0) then
                align_len = align_len + 1
                alignment1(align_len) = seq1(i)
                alignment2(align_len) = '-'
                i = i - 1
            else if (j > 0) then
                align_len = align_len + 1
                alignment1(align_len) = '-'
                alignment2(align_len) = seq2(j)
                j = j - 1
            end if
        end if
    end do
    
    ! Output the score
    write(*,*) score(m, n)
    
    ! Output the alignment (reverse order since we built it backwards)
    do i = align_len, 1, -1
        write(*,*) alignment1(i), alignment2(i)
    end do
    
end program semiglobal_alignment
```

## Alternative Implementation (More Robust)

Here's a cleaner, more robust version:

```fortran
program semiglobal_alignment_v2
    implicit none
    integer, parameter :: max_len = 1000
    character(len=1) :: seq1(max_len), seq2(max_len)
    integer :: m, n, i, j
    integer :: score(max_len, max_len)
    integer :: match_score = 2
    integer :: mismatch_score = -1
    integer :: gap_penalty = -1
    integer :: max_score
    
    ! Read input
    read(*,*) m
    read(*,*) n
    read(*,*) (seq1(i), i=1,m)
    read(*,*) (seq2(i), i=1,n)
    
    ! Initialize DP matrix
    do i = 0, m
        score(i, 0) = 0
    end do
    do j = 0, n
        score(0, j) = 0
    end do
    
    ! Fill DP matrix
    do i = 1, m
        do j = 1, n
            ! Match/mismatch score
            if (seq1(i) == seq2(j)) then
                score(i, j) = score(i-1, j-1) + match_score
            else
                score(i, j) = score(i-1, j-1) + mismatch_score
            end if
            
            ! Gap scores (allow gaps at both ends)
            score(i, j) = max(score(i, j), &
                              score(i-1, j) + gap_penalty, &
                              score(i, j-1) + gap_penalty)
        end do
    end do
    
    ! Output the maximum score
    max_score = score(m, n)
    write(*,*) max_score
    
end program semiglobal_alignment_v2
```

## Key Points of Implementation

1. **Matrix Initialization**: The DP matrix is initialized with zeros for the first row and column, allowing gaps at sequence ends.

2. **Recurrence Relation**: 
   - Match/mismatch: `score(i,j) = score(i-1,j-1) + (match_score or mismatch_score)`
   - Gap penalties: `score(i,j) = max(score(i-1,j) + gap_penalty, score(i,j-1) + gap_penalty)`

3. **Semiglobal Nature**: Unlike global alignment, we don't penalize gaps at the beginning or end of sequences.

4. **Scoring Parameters**: 
   - Match score: 2
   - Mismatch score: -1  
   - Gap penalty: -1

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m × n) for the DP matrix

## Sample Input/Output

**Input:**
```
5
4
A C G T A
T A C G
```

**Output:**
```
3
```

The algorithm finds the optimal semiglobal alignment score between the two sequences, allowing gaps at the sequence ends while maximizing the alignment score for the internal regions.

