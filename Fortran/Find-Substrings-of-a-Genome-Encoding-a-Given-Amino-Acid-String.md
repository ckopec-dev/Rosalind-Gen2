# Rosalind Problem: Find Substrings of a Genome Encoding a Given Amino Acid String (Fortran Solution)

## Problem Understanding

This problem asks us to find all substrings of a given DNA sequence that encode a specific amino acid sequence using the standard genetic code.

## Solution Approach

1. Create a mapping from codons (3-letter DNA sequences) to amino acids
2. Generate all possible DNA substrings of length 3n (where n is the length of the target amino acid sequence)
3. Translate each substring to amino acids and compare with target sequence
4. Return all matching substrings

## Fortran Implementation

```fortran
program find_substrings
    implicit none
    character(len=1), parameter :: amino_acids(0:63) = (/ &
        'K','N','K','N','T','T','T','T','R','S','R','S','I','I','M','I', &
        'Q','H','Q','H','P','P','P','P','R','R','R','R','L','L','L','L', &
        'E','D','E','D','A','A','A','A','G','G','G','G','V','V','V','V', &
        'O','Y','O','Y','S','S','S','S','C','C','O','C','W','F','W','F' /)
    
    character(len=3), parameter :: codons(0:63) = (/ &
        'AAA','AAC','AAG','AAT','ACA','ACC','ACG','ACT','AGA','AGC','AGG','AGT', &
        'ATA','ATC','ATG','ATT','CAA','CAC','CAG','CAT','CCA','CCC','CCG','CCT', &
        'CGA','CGC','CGG','CGT','CTA','CTC','CTG','CTT','GAA','GAC','GAG','GAT', &
        'GCA','GCC','GCG','GCT','GGA','GGC','GGG','GGT','GTA','GTC','GTG','GTT', &
        'TAA','TAC','TAG','TAT','TCA','TCC','TCG','TCT','TGA','TGC','TGG','TGT', &
        'TTA','TTC','TTG','TTT' /)
    
    character(len=1000) :: genome, target_aa
    character(len=3) :: codon
    character(len=100) :: amino_seq
    integer :: i, j, k, n, m, l, pos, len_genome, len_target
    logical :: found
    
    ! Read input
    read(*,*) genome
    read(*,*) target_aa
    
    len_genome = len_trim(genome)
    len_target = len_trim(target_aa)
    n = len_target
    m = 3 * n
    
    ! Check if genome is long enough
    if (len_genome < m) then
        write(*,*) "No substrings found"
        stop
    end if
    
    ! Find all substrings
    do i = 1, len_genome - m + 1
        amino_seq = ''
        found = .true.
        
        ! Translate current substring to amino acids
        do j = 1, n
            codon = genome(i + (j-1)*3:i + j*3 - 1)
            
            ! Find corresponding amino acid
            pos = 0
            do k = 0, 63
                if (codon == codons(k)) then
                    pos = k
                    exit
                end if
            end do
            
            if (pos == 0) then
                found = .false.
                exit
            end if
            
            amino_seq(j:j) = amino_acids(pos)
        end do
        
        ! Check if matches target
        if (found .and. trim(amino_seq) == trim(target_aa)) then
            write(*,*) genome(i:i+m-1)
        end if
    end do
    
end program find_substrings
```

## Alternative Implementation with Better Structure

```fortran
program find_substrings_improved
    implicit none
    character(len=1), parameter :: amino_acids(0:63) = (/ &
        'K','N','K','N','T','T','T','T','R','S','R','S','I','I','M','I', &
        'Q','H','Q','H','P','P','P','P','R','R','R','R','L','L','L','L', &
        'E','D','E','D','A','A','A','A','G','G','G','G','V','V','V','V', &
        'O','Y','O','Y','S','S','S','S','C','C','O','C','W','F','W','F' /)
    
    character(len=3), parameter :: codons(0:63) = (/ &
        'AAA','AAC','AAG','AAT','ACA','ACC','ACG','ACT','AGA','AGC','AGG','AGT', &
        'ATA','ATC','ATG','ATT','CAA','CAC','CAG','CAT','CCA','CCC','CCG','CCT', &
        'CGA','CGC','CGG','CGT','CTA','CTC','CTG','CTT','GAA','GAC','GAG','GAT', &
        'GCA','GCC','GCG','GCT','GGA','GGC','GGG','GGT','GTA','GTC','GTG','GTT', &
        'TAA','TAC','TAG','TAT','TCA','TCC','TCG','TCT','TGA','TGC','TGG','TGT', &
        'TTA','TTC','TTG','TTT' /)
    
    character(len=1000) :: genome, target_aa
    character(len=3) :: codon
    character(len=100) :: amino_seq
    integer :: i, j, k, n, m, pos, len_genome, len_target
    logical :: match
    
    ! Read input
    read(*,*) genome
    read(*,*) target_aa
    
    len_genome = len_trim(genome)
    len_target = len_trim(target_aa)
    n = len_target
    m = 3 * n
    
    ! Check if genome is long enough
    if (len_genome < m) then
        write(*,*) "No substrings found"
        stop
    end if
    
    ! Find all substrings that encode target amino acid sequence
    do i = 1, len_genome - m + 1
        amino_seq = ' '
        match = .true.
        
        ! Translate current substring to amino acids
        do j = 1, n
            codon = genome(i + (j-1)*3:i + j*3 - 1)
            
            ! Find corresponding amino acid
            pos = 0
            do k = 0, 63
                if (codon == codons(k)) then
                    pos = k
                    exit
                end if
            end do
            
            ! If codon not found in genetic code, skip this substring
            if (pos == 0) then
                match = .false.
                exit
            end if
            
            amino_seq(j:j) = amino_acids(pos)
        end do
        
        ! Check if translation matches target amino acid sequence
        if (match .and. trim(amino_seq) == trim(target_aa)) then
            write(*,*) genome(i:i+m-1)
        end if
    end do
    
end program find_substrings_improved
```

## Input Format

The program expects two lines of input:
1. First line: DNA sequence (genome)
2. Second line: Target amino acid sequence

## Example Usage

Input:
```
ATGGCCATGGCCCCC
MA
```

Output:
```
ATGGCC
ATGGCCATGG
```

## Key Features

1. **Genetic Code Mapping**: Uses standard genetic code table mapping codons to amino acids
2. **Substring Generation**: Checks all possible substrings of length 3n in the genome
3. **Translation**: Translates DNA substrings to amino acid sequences
4. **Comparison**: Matches translated sequences with target amino acid string
5. **Output**: Prints all matching DNA substrings

## Time Complexity

- O(n × m) where n is the length of the genome and m is the length of the target amino acid sequence
- For each position in the genome, we perform translation of m codons

## Space Complexity

- O(1) additional space (excluding input/output) for the genetic code mapping
- O(m) for storing the translated amino acid sequence during comparison

