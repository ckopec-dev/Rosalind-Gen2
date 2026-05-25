# Rosalind Problem: Translate an RNA String into an Amino Acid String (Fortran Solution)

## Problem Understanding

The task is to translate an RNA string into an amino acid string using the standard genetic code. We need to:
1. Read an RNA string from input
2. Translate each codon (3-character substring) into its corresponding amino acid
3. Stop translation when encountering a stop codon (UAA, UAG, or UGA)

## Solution

```fortran
program translate_rna
    implicit none
    character(len=1), parameter :: STOP = 'X'
    character(len=1), parameter :: EMPTY = ' '
    character(len=1000) :: rna_string
    character(len=3) :: codon
    integer :: i, j, string_length
    logical :: stop_found
    
    ! Read the RNA string
    read(*, '(A)') rna_string
    
    ! Get the length of the RNA string
    string_length = len_trim(rna_string)
    
    ! Initialize variables
    stop_found = .false.
    
    ! Process the RNA string in groups of 3 characters (codons)
    do i = 1, string_length, 3
        ! Check if we have a complete codon
        if (i + 2 <= string_length) then
            codon = rna_string(i:i+2)
            
            ! Translate the codon to amino acid
            select case (codon)
                case ('UUU', 'UUC')
                    write(*, '(A)', advance='no') 'F'
                case ('UUA', 'UUG')
                    write(*, '(A)', advance='no') 'L'
                case ('CUU', 'CUC', 'CUA', 'CUG')
                    write(*, '(A)', advance='no') 'L'
                case ('AUU', 'AUC', 'AUA')
                    write(*, '(A)', advance='no') 'I'
                case ('AUG')
                    write(*, '(A)', advance='no') 'M'
                case ('GUU', 'GUC', 'GUA', 'GUG')
                    write(*, '(A)', advance='no') 'V'
                case ('UCU', 'UCC', 'UCA', 'UCG')
                    write(*, '(A)', advance='no') 'S'
                case ('CCU', 'CCC', 'CCA', 'CCG')
                    write(*, '(A)', advance='no') 'P'
                case ('ACU', 'ACC', 'ACA', 'ACG')
                    write(*, '(A)', advance='no') 'T'
                case ('GCU', 'GCC', 'GCA', 'GCG')
                    write(*, '(A)', advance='no') 'A'
                case ('UAU', 'UAC')
                    write(*, '(A)', advance='no') 'Y'
                case ('UAA', 'UAG', 'UGA')
                    ! Stop codon - stop translation
                    stop_found = .true.
                    exit
                case ('CAU', 'CAC')
                    write(*, '(A)', advance='no') 'H'
                case ('CAA', 'CAG')
                    write(*, '(A)', advance='no') 'Q'
                case ('AAU', 'AAC')
                    write(*, '(A)', advance='no') 'N'
                case ('AAA', 'AAG')
                    write(*, '(A)', advance='no') 'K'
                case ('GAU', 'GAC')
                    write(*, '(A)', advance='no') 'D'
                case ('GAA', 'GAG')
                    write(*, '(A)', advance='no') 'E'
                case ('UGU', 'UGC')
                    write(*, '(A)', advance='no') 'C'
                case ('UGG')
                    write(*, '(A)', advance='no') 'W'
                case ('CGU', 'CGC', 'CGA', 'CGG', 'AGA', 'AGG')
                    write(*, '(A)', advance='no') 'R'
                case ('AGU', 'AGC')
                    write(*, '(A)', advance='no') 'S'
                case ('GGU', 'GGC', 'GGA', 'GGG')
                    write(*, '(A)', advance='no') 'G'
                case default
                    ! Handle unknown codons
                    write(*, '(A)', advance='no') '?'
            end select
            
            ! If we found a stop codon, exit the loop
            if (stop_found) then
                exit
            end if
        else
            ! Incomplete codon at the end - should not happen in valid input
            write(*, '(A)', advance='no') '?'
        end if
    end do
    
    ! New line at the end
    write(*, *)
    
end program translate_rna
```

## How to Use

1. Save the code to a file named `translate_rna.f90`
2. Compile with a Fortran compiler: `gfortran translate_rna.f90 -o translate_rna`
3. Run the program with RNA input: `echo "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA" | ./translate_rna`

## Expected Output

For the example input "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA", the output should be:
```
MAMAPRTEINSTRING
```

## Explanation

The program:
1. Reads the RNA string from standard input
2. Processes the string in groups of 3 characters (codons)
3. Uses a `select case` statement to translate each codon to its corresponding amino acid
4. Stops translation when encountering a stop codon (UAA, UAG, or UGA)
5. Outputs the resulting amino acid sequence

The genetic code mapping follows the standard table where each codon corresponds to a specific amino acid, with special handling for stop codons that terminate translation.

