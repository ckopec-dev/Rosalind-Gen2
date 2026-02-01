# Rosalind Problem: Convert a Peptide into a Peptide Vector (Cobol Solution)

## Problem Understanding

The task is to convert a peptide string into its corresponding peptide vector representation. The peptide vector is a binary vector where:
- Each position represents an amino acid
- 1 indicates the presence of that amino acid
- 0 indicates absence

## Solution Approach

I'll implement a COBOL program that:
1. Takes a peptide string as input
2. Converts each amino acid to its numeric representation
3. Creates a binary vector of size 20 (for 20 standard amino acids)
4. Sets the corresponding positions to 1

## COBOL Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEPTIDE-VECTOR-CONVERTER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "PEPTIDE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "VECTOR.OUT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-PEPTIDE PIC A(100).

       FD OUTPUT-FILE.
       01 OUTPUT-VECTOR PIC A(200).

       WORKING-STORAGE SECTION.
       01 PEPTIDE-STRING PIC A(100).
       01 VECTOR-ARRAY OCCURS 20 TIMES PIC 9 VALUE 0.
       01 I PIC 99 VALUE 1.
       01 J PIC 99 VALUE 1.
       01 LENGTH-OF-PEPTIDE PIC 99 VALUE 0.
       01 CURRENT-AMINO-ACID PIC A(1).
       01 OUTPUT-STRING PIC A(200).
       01 SPACE-CHAR PIC A VALUE ' '.
       01 NEWLINE PIC A VALUE X'0A'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO PEPTIDE-STRING
               AT END GO TO END-PROGRAM
           END-READ

           MOVE PEPTIDE-STRING TO INPUT-PEPTIDE
           COMPUTE LENGTH-OF-PEPTIDE = FUNCTION LENGTH(INPUT-PEPTIDE)

           PERFORM CONVERT-PEPTIDE-TO-VECTOR

           PERFORM WRITE-OUTPUT

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       CONVERT-PEPTIDE-TO-VECTOR.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH-OF-PEPTIDE
               MOVE INPUT-PEPTIDE(I:1) TO CURRENT-AMINO-ACID
               PERFORM GET-AMINO-ACID-INDEX
               IF I-INDEX > 0
                   MOVE 1 TO VECTOR-ARRAY(I-INDEX)
               END-IF
           END-PERFORM.

       GET-AMINO-ACID-INDEX.
           COMPUTE I-INDEX = 0.
           IF CURRENT-AMINO-ACID = 'A' THEN MOVE 1 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'C' THEN MOVE 2 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'D' THEN MOVE 3 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'E' THEN MOVE 4 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'F' THEN MOVE 5 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'G' THEN MOVE 6 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'H' THEN MOVE 7 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'I' THEN MOVE 8 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'K' THEN MOVE 9 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'L' THEN MOVE 10 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'M' THEN MOVE 11 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'N' THEN MOVE 12 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'P' THEN MOVE 13 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'Q' THEN MOVE 14 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'R' THEN MOVE 15 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'S' THEN MOVE 16 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'T' THEN MOVE 17 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'V' THEN MOVE 18 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'W' THEN MOVE 19 TO I-INDEX
           ELSE IF CURRENT-AMINO-ACID = 'Y' THEN MOVE 20 TO I-INDEX
           END-IF.

       WRITE-OUTPUT.
           MOVE SPACES TO OUTPUT-STRING
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 20
               STRING VECTOR-ARRAY(J) DELIMITED BY SIZE
                       SPACE-CHAR DELIMITED BY SIZE
                       INTO OUTPUT-STRING
               END-STRING
           END-PERFORM.

           WRITE OUTPUT-VECTOR FROM OUTPUT-STRING
           END-WRITE.

       END-PROGRAM.
```

## Alternative Implementation (More Compact)

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEPTIDE-VECTOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "PEPTIDE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 PEPTIDE-INPUT PIC A(100).

       WORKING-STORAGE SECTION.
       01 PEPTIDE-STRING PIC A(100).
       01 PEPTIDE-VECTOR OCCURS 20 TIMES PIC 9 VALUE 0.
       01 I PIC 99 VALUE 1.
       01 LENGTH PIC 99 VALUE 0.
       01 AMINO-ACID PIC A(1).
       01 INDEX PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO PEPTIDE-STRING
           CLOSE INPUT-FILE

           COMPUTE LENGTH = FUNCTION LENGTH(PEPTIDE-STRING)

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH
               MOVE PEPTIDE-STRING(I:1) TO AMINO-ACID
               EVALUATE AMINO-ACID
                   WHEN 'A' MOVE 1 TO INDEX
                   WHEN 'C' MOVE 2 TO INDEX
                   WHEN 'D' MOVE 3 TO INDEX
                   WHEN 'E' MOVE 4 TO INDEX
                   WHEN 'F' MOVE 5 TO INDEX
                   WHEN 'G' MOVE 6 TO INDEX
                   WHEN 'H' MOVE 7 TO INDEX
                   WHEN 'I' MOVE 8 TO INDEX
                   WHEN 'K' MOVE 9 TO INDEX
                   WHEN 'L' MOVE 10 TO INDEX
                   WHEN 'M' MOVE 11 TO INDEX
                   WHEN 'N' MOVE 12 TO INDEX
                   WHEN 'P' MOVE 13 TO INDEX
                   WHEN 'Q' MOVE 14 TO INDEX
                   WHEN 'R' MOVE 15 TO INDEX
                   WHEN 'S' MOVE 16 TO INDEX
                   WHEN 'T' MOVE 17 TO INDEX
                   WHEN 'V' MOVE 18 TO INDEX
                   WHEN 'W' MOVE 19 TO INDEX
                   WHEN 'Y' MOVE 20 TO INDEX
                   WHEN OTHER MOVE 0 TO INDEX
               END-EVALUATE
               IF INDEX > 0
                   MOVE 1 TO PEPTIDE-VECTOR(INDEX)
               END-IF
           END-PERFORM

           DISPLAY "Peptide Vector:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 20
               DISPLAY PEPTIDE-VECTOR(I) WITH NO ADVANCING
           END-PERFORM
           DISPLAY ""
           STOP RUN.
```

## Example Usage

**Input file (PEPTIDE.TXT):**
```
SKADYEK
```

**Expected Output:**
```
Peptide Vector:
01001000000000000000
```

## Explanation

The program:
1. Reads a peptide string from input
2. Processes each amino acid character
3. Maps each amino acid to its position in the 20-amino acid alphabet
4. Sets the corresponding vector position to 1
5. Outputs the binary vector representation

The mapping is:
- A=1, C=2, D=3, E=4, F=5, G=6, H=7, I=8, K=9, L=10
- M=11, N=12, P=13, Q=14, R=15, S=16, T=17, V=18, W=19, Y=20

This creates a binary vector where 1s indicate the presence of specific amino acids in the peptide sequence.

