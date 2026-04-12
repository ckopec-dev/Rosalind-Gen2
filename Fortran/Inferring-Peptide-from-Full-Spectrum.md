# Rosalind Problem: Inferring Peptide from Full Spectrum (Fortran Solution)

## Problem Understanding

This problem asks us to determine the peptide sequence from a full spectrum mass spectrum. We need to:
1. Parse the spectrum data
2. Find the peptide that matches the given spectrum
3. Return the amino acid sequence

## Solution Approach

I'll use dynamic programming to find the peptide sequence by:
- Creating a mapping of mass to amino acids
- Using backtracking to reconstruct the peptide
- Handling the spectrum matching

## Fortran Implementation

```fortran
program infer_peptide_from_spectrum
    implicit none
    integer, parameter :: MAX_N = 1000
    integer, parameter :: MAX_MASS = 10000
    integer :: n, i, j, k, target_mass
    real :: spectrum(MAX_N)
    real :: mass_table(20) = [57.0, 71.0, 87.0, 97.0, 99.0, 101.0, 103.0, 113.0, 114.0, &
                              115.0, 128.0, 129.0, 131.0, 137.0, 147.0, 156.0, 163.0, 186.0, &
                              113.0, 128.0]
    character(len=1) :: amino_acids(20) = ['A', 'C', 'D', 'E', 'F', 'G', 'H', 'I', &
                                          'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', &
                                          'T', 'V', 'W', 'Y']
    integer :: parent_mass
    integer :: dp(MAX_MASS)
    integer :: backtrack(MAX_MASS)
    character(len=1000) :: result
    character(len=1000) :: line
    
    ! Read the spectrum
    n = 0
    do while (.true.)
        read(*, '(A)', iostat=i) line
        if (i /= 0) exit
        n = n + 1
        read(line, *) spectrum(n)
    end do
    
    ! Find parent mass (maximum value in spectrum)
    parent_mass = int(maxval(spectrum))
    
    ! Initialize DP arrays
    do i = 1, MAX_MASS
        dp(i) = 0
        backtrack(i) = 0
    end do
    
    ! Set base case
    dp(0) = 1
    
    ! Dynamic programming to find valid combinations
    do i = 1, n-1
        do j = parent_mass, int(spectrum(i)) + 1, -1
            if (dp(j - int(spectrum(i))) > 0) then
                dp(j) = dp(j) + dp(j - int(spectrum(i)))
                if (dp(j) > 0) backtrack(j) = int(spectrum(i))
            end if
        end do
    end do
    
    ! Reconstruct the peptide sequence
    result = ''
    target_mass = parent_mass
    do while (target_mass > 0)
        ! Find which amino acid was used
        do i = 1, 20
            if (int(mass_table(i)) == backtrack(target_mass)) then
                result = amino_acids(i) // result
                target_mass = target_mass - int(mass_table(i))
                exit
            end if
        end do
    end do
    
    ! Output the result
    write(*,*) trim(result)
    
end program infer_peptide_from_spectrum
```

## Alternative Implementation (More Robust)

```fortran
program infer_peptide_from_spectrum_v2
    implicit none
    integer, parameter :: MAX_N = 1000
    integer, parameter :: MAX_MASS = 10000
    integer :: n, i, j, k, target_mass
    real :: spectrum(MAX_N)
    real :: mass_table(20) = [57.0, 71.0, 87.0, 97.0, 99.0, 101.0, 103.0, 113.0, 114.0, &
                              115.0, 128.0, 129.0, 131.0, 137.0, 147.0, 156.0, 163.0, 186.0, &
                              113.0, 128.0]
    character(len=1) :: amino_acids(20) = ['A', 'C', 'D', 'E', 'F', 'G', 'H', 'I', &
                                          'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', &
                                          'T', 'V', 'W', 'Y']
    integer :: parent_mass
    integer :: dp(MAX_MASS)
    integer :: backtrack(MAX_MASS)
    character(len=1000) :: result
    character(len=1000) :: line
    integer :: spectrum_masses(MAX_N)
    integer :: num_masses
    
    ! Read the spectrum
    n = 0
    do while (.true.)
        read(*, '(A)', iostat=i) line
        if (i /= 0) exit
        n = n + 1
        read(line, *) spectrum(n)
    end do
    
    ! Convert spectrum to integer masses and find parent mass
    parent_mass = 0
    do i = 1, n
        spectrum_masses(i) = int(spectrum(i))
        if (spectrum_masses(i) > parent_mass) parent_mass = spectrum_masses(i)
    end do
    num_masses = n
    
    ! Initialize DP arrays
    do i = 1, MAX_MASS
        dp(i) = 0
        backtrack(i) = 0
    end do
    
    ! Base case
    dp(0) = 1
    
    ! Dynamic programming - for each mass in spectrum
    do i = 1, num_masses
        ! Process from high to low to avoid double counting
        do j = parent_mass, spectrum_masses(i) + 1, -1
            if (dp(j - spectrum_masses(i)) > 0) then
                dp(j) = dp(j) + dp(j - spectrum_masses(i))
                if (dp(j) > 0) backtrack(j) = spectrum_masses(i)
            end if
        end do
    end do
    
    ! Reconstruct the peptide sequence
    result = ''
    target_mass = parent_mass
    
    ! Backtrack to find the sequence
    do while (target_mass > 0)
        ! Find which amino acid was used
        do i = 1, 20
            if (int(mass_table(i)) == backtrack(target_mass)) then
                result = amino_acids(i) // result
                target_mass = target_mass - int(mass_table(i))
                exit
            end if
        end do
    end do
    
    ! Output the result
    write(*,*) trim(result)
    
end program infer_peptide_from_spectrum_v2
```

## Key Points

1. **Input Processing**: Reads the spectrum from standard input until EOF
2. **Mass Table**: Uses standard amino acid masses for the mapping
3. **Dynamic Programming**: Builds up valid combinations of masses
4. **Backtracking**: Reconstructs the actual peptide sequence
5. **Output**: Returns the peptide sequence as a string

## Time Complexity
- O(P × N) where P is the parent mass and N is the number of masses in the spectrum

## Space Complexity
- O(P) for the DP arrays

This solution handles the core requirements of inferring a peptide from its full spectrum mass spectrum using dynamic programming and backtracking techniques.

