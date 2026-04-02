# Rosalind Problem: Transcribing DNA into RNA in COBOL

## Problem Statement
Given a DNA string, return the transcribed RNA string (replace all 'T' with 'U').

## Solution in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSCRIBING-DNA-INTO-RNA.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO "rosalind_rna.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE.
       01 INPUT-RECORD PIC X(1000).

       FD OUT-FILE.
       01 OUTPUT-RECORD PIC X(1000).

       WORKING-STORAGE SECTION.
       01 DNA-STRING PIC X(1000).
       01 RNA-STRING PIC X(1000).
       01 I PIC 9(4) VALUE 1.
       01 J PIC 9(4) VALUE 1.
       01 RECORD-LENGTH PIC 9(4).
       01 EOF-FLAG PIC X VALUE 'N'.
           88 END-OF-FILE VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT IN-FILE
                OUTPUT OUT-FILE

           READ IN-FILE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ

           IF END-OF-FILE
               GO TO END-PROGRAM
           END-IF

           MOVE INPUT-RECORD TO DNA-STRING
           COMPUTE RECORD-LENGTH = FUNCTION LENGTH(DNA-STRING)

           PERFORM TRANSCRIBE-DNA-TO-RNA

           WRITE OUTPUT-RECORD FROM RNA-STRING

           CLOSE IN-FILE
           CLOSE OUT-FILE

           STOP RUN.

       TRANSCRIBE-DNA-TO-RNA.
           MOVE SPACES TO RNA-STRING
           MOVE 1 TO J

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > RECORD-LENGTH
               IF DNA-STRING(I:1) = 'T'
                   MOVE 'U' TO RNA-STRING(J:1)
               ELSE
                   MOVE DNA-STRING(I:1) TO RNA-STRING(J:1)
               END-IF
               ADD 1 TO J
           END-PERFORM.

       END-PROGRAM.
```

## Explanation

This COBOL program solves the DNA to RNA transcription problem by:

1. **File Handling**: 
   - Opens input file "rosalind_rna.dat" for reading
   - Opens output file "output.txt" for writing

2. **Data Processing**:
   - Reads the DNA string from input file
   - Processes each character in the DNA string
   - Replaces 'T' with 'U' to create RNA string
   - Preserves all other characters unchanged

3. **Algorithm**:
   - Uses a loop to iterate through each character of the DNA string
   - Checks if current character is 'T'
   - If 'T', replaces with 'U' in the RNA string
   - Otherwise, copies the character as-is
   - Maintains position tracking for both input and output strings

## Sample Input/Output

**Input (rosalind_rna.dat)**:
```
GATGGAACTTGACTACGTAAATT
```

**Output (output.txt)**:
```
GAUGGAACUUGACUACGUAAAUU
```

## Key COBOL Features Used

- **File Processing**: Sequential file handling with OPEN, READ, and WRITE operations
- **String Manipulation**: Character-by-character processing using substring notation (I:1)
- **Control Structures**: PERFORM VARYING loop for iteration
- **Conditional Logic**: IF-ELSE statements for character replacement
- **Data Conversion**: LENGTH function to determine string length

The program handles DNA strings of up to 1000 characters and efficiently converts all thymine ('T') nucleotides to uracil ('U') nucleotides as required for RNA transcription.

