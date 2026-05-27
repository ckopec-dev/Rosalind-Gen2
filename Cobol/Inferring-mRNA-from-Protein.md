# Rosalind Problem: Inferring_mRNA_from_Protein (COBOL Solution)

## Problem Understanding

This problem asks us to calculate how many different mRNA sequences can code for a given protein sequence, considering that some amino acids have multiple codons (three-letter RNA sequences). We need to account for the fact that there are 64 possible codons (4³) but only 20 amino acids, so some amino acids are coded by multiple codons.

## Solution Approach

1. Create a mapping of amino acids to their codon counts
2. For each amino acid in the protein sequence, multiply the result by the number of possible codons for that amino acid
3. Account for the stop codons (which are 3 in total)
4. Return the result modulo 1,000,000

## COBOL Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INFERRING-MRNA-FROM-PROTEIN.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_mrna.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(1000).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 PROTEIN-SEQUENCE PIC X(1000).
       01 PROTEIN-LENGTH PIC 9(4).
       01 I PIC 9(4).
       01 AMINO-ACID PIC X(1).
       01 RESULT PIC 9(10) VALUE 1.
       01 MODULO PIC 9(10) VALUE 1000000.
       01 TEMP PIC 9(10).
       01 CODON-COUNT PIC 9(2).
       01 STOP-CODONS PIC 9(2) VALUE 3.
       01 EOF-FLAG PIC X VALUE 'N'.

       01 CODON-TABLE.
           02 CODON-ENTRY PIC X(1) OCCURS 20 TIMES.
           02 CODON-COUNTS PIC 9(2) OCCURS 20 TIMES.

       01 AMINO-ACIDS.
           02 A PIC X VALUE 'A'.
           02 R PIC X VALUE 'R'.
           02 N PIC X VALUE 'N'.
           02 D PIC X VALUE 'D'.
           02 C PIC X VALUE 'C'.
           02 Q PIC X VALUE 'Q'.
           02 E PIC X VALUE 'E'.
           02 G PIC X VALUE 'G'.
           02 H PIC X VALUE 'H'.
           02 I PIC X VALUE 'I'.
           02 L PIC X VALUE 'L'.
           02 K PIC X VALUE 'K'.
           02 M PIC X VALUE 'M'.
           02 F PIC X VALUE 'F'.
           02 P PIC X VALUE 'P'.
           02 S PIC X VALUE 'S'.
           02 T PIC X VALUE 'T'.
           02 W PIC X VALUE 'W'.
           02 Y PIC X VALUE 'Y'.
           02 V PIC X VALUE 'V'.

       01 CODON-COUNTS-VALUES.
           02 A-COUNT PIC 9(2) VALUE 4.
           02 R-COUNT PIC 9(2) VALUE 6.
           02 N-COUNT PIC 9(2) VALUE 2.
           02 D-COUNT PIC 9(2) VALUE 2.
           02 C-COUNT PIC 9(2) VALUE 2.
           02 Q-COUNT PIC 9(2) VALUE 2.
           02 E-COUNT PIC 9(2) VALUE 2.
           02 G-COUNT PIC 9(2) VALUE 4.
           02 H-COUNT PIC 9(2) VALUE 2.
           02 I-COUNT PIC 9(2) VALUE 3.
           02 L-COUNT PIC 9(2) VALUE 6.
           02 K-COUNT PIC 9(2) VALUE 2.
           02 M-COUNT PIC 9(2) VALUE 1.
           02 F-COUNT PIC 9(2) VALUE 2.
           02 P-COUNT PIC 9(2) VALUE 4.
           02 S-COUNT PIC 9(2) VALUE 6.
           02 T-COUNT PIC 9(2) VALUE 4.
           02 W-COUNT PIC 9(2) VALUE 1.
           02 Y-COUNT PIC 9(2) VALUE 2.
           02 V-COUNT PIC 9(2) VALUE 4.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO PROTEIN-SEQUENCE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ

           IF EOF-FLAG = 'N'
               MOVE FUNCTION LENGTH(PROTEIN-SEQUENCE) TO PROTEIN-LENGTH
               MOVE 1 TO RESULT

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > PROTEIN-LENGTH
                   MOVE PROTEIN-SEQUENCE(I:1) TO AMINO-ACID
                   PERFORM GET-CODON-COUNT
                   COMPUTE TEMP = (RESULT * CODON-COUNT) MOD MODULO
                   MOVE TEMP TO RESULT
               END-PERFORM

               COMPUTE TEMP = RESULT * STOP-CODONS MOD MODULO
               MOVE TEMP TO RESULT

               MOVE RESULT TO OUTPUT-RECORD
               STRING OUTPUT-RECORD " " INTO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       GET-CODON-COUNT.
           EVALUATE AMINO-ACID
               WHEN 'A' MOVE A-COUNT TO CODON-COUNT
               WHEN 'R' MOVE R-COUNT TO CODON-COUNT
               WHEN 'N' MOVE N-COUNT TO CODON-COUNT
               WHEN 'D' MOVE D-COUNT TO CODON-COUNT
               WHEN 'C' MOVE C-COUNT TO CODON-COUNT
               WHEN 'Q' MOVE Q-COUNT TO CODON-COUNT
               WHEN 'E' MOVE E-COUNT TO CODON-COUNT
               WHEN 'G' MOVE G-COUNT TO CODON-COUNT
               WHEN 'H' MOVE H-COUNT TO CODON-COUNT
               WHEN 'I' MOVE I-COUNT TO CODON-COUNT
               WHEN 'L' MOVE L-COUNT TO CODON-COUNT
               WHEN 'K' MOVE K-COUNT TO CODON-COUNT
               WHEN 'M' MOVE M-COUNT TO CODON-COUNT
               WHEN 'F' MOVE F-COUNT TO CODON-COUNT
               WHEN 'P' MOVE P-COUNT TO CODON-COUNT
               WHEN 'S' MOVE S-COUNT TO CODON-COUNT
               WHEN 'T' MOVE T-COUNT TO CODON-COUNT
               WHEN 'W' MOVE W-COUNT TO CODON-COUNT
               WHEN 'Y' MOVE Y-COUNT TO CODON-COUNT
               WHEN 'V' MOVE V-COUNT TO CODON-COUNT
               WHEN OTHER MOVE 0 TO CODON-COUNT
           END-EVALUATE.
```

## Explanation

This COBOL solution works as follows:

1. **File Handling**: Opens input and output files for reading and writing
2. **Data Reading**: Reads the protein sequence from input file
3. **Main Logic**: 
   - Initializes result to 1
   - For each amino acid in the protein sequence:
     - Determines the number of possible codons for that amino acid
     - Multiplies the current result by the codon count
     - Takes modulo 1,000,000 to prevent overflow
4. **Final Adjustment**: Multiplies by 3 for stop codons
5. **Output**: Writes the final result to output file

## Key Features

- **Modular Design**: Uses separate paragraphs for different functions
- **Error Handling**: Checks for end-of-file conditions
- **Modular Arithmetic**: Uses modulo operation to handle large numbers
- **Codon Count Table**: Pre-defined counts for all 20 amino acids
- **Stop Codon Handling**: Multiplies by 3 for the three stop codons

## Time Complexity
O(n) where n is the length of the protein sequence

## Space Complexity  
O(1) - uses constant extra space

The solution correctly handles the mathematical requirement of counting all possible mRNA sequences that could code for a given protein, considering the degeneracy of the genetic code.

