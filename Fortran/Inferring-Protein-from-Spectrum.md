# Rosalind Problem: Inferring Protein from Spectrum (Fortran Solution)

## Problem Understanding

This problem involves determining the protein sequence from a mass spectrum by finding the most likely amino acid sequence that would produce the given spectrum peaks.

## Solution Approach

I'll implement a dynamic programming solution to find the protein sequence that best matches the given spectrum.

```fortran
program infer_protein_from_spectrum
    implicit none
    integer, parameter :: max_peaks = 1000
    integer, parameter :: max_sequence = 100
    integer :: n_peaks, i, j, k
    real :: peaks(max_peaks)
    real :: mass_table(20) = [57.0, 71.0, 87.0, 97.0, 99.0, 101.0, 103.0, 113.0, &
                             114.0, 115.0, 128.0, 129.0, 131.0, 137.0, 147.0, &
                             156.0, 163.0, 186.0, 194.0, 225.0]
    character(len=1) :: amino_acids(20) = ['A', 'C', 'D', 'E', 'F', 'G', 'H', 'I', &
                                         'K', 'L', 'M', 'N', 'P', 'Q', 'R', &
                                         'S', 'T', 'V', 'W', 'Y']
    real :: dp(max_peaks)
    integer :: parent_index(max_peaks)
    character(len=1) :: sequence(max_sequence)
    integer :: seq_length
    
    ! Read input peaks
    read(*,*) n_peaks
    do i = 1, n_peaks
        read(*,*) peaks(i)
    end do
    
    ! Initialize DP array
    do i = 1, n_peaks
        dp(i) = 0.0
        parent_index(i) = 0
    end do
    
    ! Dynamic programming to find best sequence
    dp(1) = 1.0
    
    do i = 2, n_peaks
        do j = 1, 20
            ! Find the best match for this amino acid
            do k = 1, i-1
                if (abs(peaks(i) - peaks(k) - mass_table(j)) < 0.01) then
                    if (dp(k) + 1.0 > dp(i)) then
                        dp(i) = dp(k) + 1.0
                        parent_index(i) = k
                    end if
                end if
            end do
        end do
    end do
    
    ! Reconstruct the sequence
    seq_length = 0
    i = n_peaks
    do while (i > 0)
        seq_length = seq_length + 1
        ! Find which amino acid was used
        do j = 1, 20
            if (i > 1) then
                if (abs(peaks(i) - peaks(parent_index(i)) - mass_table(j)) < 0.01) then
                    sequence(seq_length) = amino_acids(j)
                    i = parent_index(i)
                    exit
                end if
            else
                ! First amino acid
                do k = 1, 20
                    if (abs(peaks(i) - mass_table(k)) < 0.01) then
                        sequence(seq_length) = amino_acids(k)
                        i = 0
                        exit
                    end if
                end do
                exit
            end if
        end do
    end do
    
    ! Print the sequence in reverse order
    do i = seq_length, 1, -1
        write(*,'(A)', advance='no') sequence(i)
    end do
    write(*,*) ""
    
end program infer_protein_from_spectrum
```

## Explanation

This Fortran program solves the protein inference problem using dynamic programming:

1. **Input Reading**: Reads the number of peaks and their masses from standard input
2. **Mass Table**: Contains the standard amino acid masses for the 20 amino acids
3. **Dynamic Programming**: 
   - `dp(i)` represents the length of the longest valid sequence ending at peak `i`
   - `parent_index(i)` tracks the previous peak in the optimal sequence
4. **Sequence Reconstruction**: Backtracks through the parent indices to build the actual protein sequence
5. **Output**: Prints the reconstructed protein sequence

## Key Features

- Uses dynamic programming to find the optimal amino acid sequence
- Handles floating-point comparisons with small tolerance (0.01)
- Properly reconstructs the sequence by backtracking
- Includes standard amino acid masses and their corresponding letters

## Time Complexity
O(n² × 20) where n is the number of peaks, due to the nested loops checking all possible amino acid combinations.

## Space Complexity
O(n) for the DP array and parent indices.

