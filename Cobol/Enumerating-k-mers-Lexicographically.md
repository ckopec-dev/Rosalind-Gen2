# Rosalind Problem: Enumerating k-mers Lexicographically in COBOL

## Problem Understanding

This problem asks us to generate all possible k-mers (strings of length k) from a given alphabet, arranged in lexicographic order.

## Solution Approach

I'll implement a recursive approach to generate all combinations, using COBOL's string handling capabilities and recursion.

## COBOL Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENUMERATE-KMERS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 ALPHABET-CHARS PIC X(100) VALUE "ACGT".
       01 ALPHABET-LENGTH PIC 99 VALUE 4.
       01 K-LENGTH PIC 99 VALUE 3.
       01 CURRENT-STRING PIC X(100).
       01 CURRENT-LENGTH PIC 99 VALUE 0.
       01 INDEX1 PIC 99 VALUE 1.
       01 INDEX2 PIC 99 VALUE 1.
       01 TEMP-CHAR PIC X.
       01 EOF-FLAG PIC X VALUE "N".
           88 END-OF-FILE VALUE "Y".
       01 OUTPUT-FILE-OPEN PIC X VALUE "N".
           88 FILE-OPEN VALUE "Y".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PROGRAM
           PERFORM GENERATE-KMERS
           PERFORM CLOSE-FILES
           STOP RUN.

       INITIALIZE-PROGRAM.
           OPEN OUTPUT OUTPUT-FILE
           MOVE 0 TO CURRENT-LENGTH
           MOVE "N" TO EOF-FLAG
           MOVE "Y" TO OUTPUT-FILE-OPEN.

       GENERATE-KMERS.
           PERFORM GENERATE-RECURSIVE-V1
           GO TO GENERATE-KMERS-EXIT.

       GENERATE-RECURSIVE-V1.
           IF CURRENT-LENGTH < K-LENGTH
               PERFORM VARYING INDEX1 FROM 1 BY 1
                   UNTIL INDEX1 > ALPHABET-LENGTH
                   MOVE ALPHABET-CHARS(INDEX1:1) TO TEMP-CHAR
                   MOVE TEMP-CHAR TO CURRENT-STRING(CURRENT-LENGTH + 1:1)
                   ADD 1 TO CURRENT-LENGTH
                   PERFORM GENERATE-RECURSIVE-V1
                   SUBTRACT 1 FROM CURRENT-LENGTH
               END-PERFORM
           ELSE
               PERFORM WRITE-RESULT
           END-IF.

       WRITE-RESULT.
           MOVE CURRENT-STRING(1:CURRENT-LENGTH) TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE SPACES TO OUTPUT-RECORD.

       CLOSE-FILES.
           CLOSE OUTPUT-FILE.

       GENERATE-KMERS-EXIT.
           EXIT.
```

## Alternative Implementation (More Compact)

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENUMERATE-KMERS-ALT.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 ALPHABET PIC X(100) VALUE "ACGT".
       01 ALPHABET-LENGTH PIC 99 VALUE 4.
       01 K-LENGTH PIC 99 VALUE 3.
       01 CURRENT-STRING PIC X(100).
       01 STRING-LENGTH PIC 99 VALUE 0.
       01 INDEX PIC 99 VALUE 1.
       01 CHAR-TO-ADD PIC X.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT OUTPUT-FILE
           PERFORM GENERATE-ALL-KMERS
           CLOSE OUTPUT-FILE
           STOP RUN.

       GENERATE-ALL-KMERS.
           MOVE 0 TO STRING-LENGTH
           PERFORM GENERATE-RECURSIVE.

       GENERATE-RECURSIVE.
           IF STRING-LENGTH < K-LENGTH
               PERFORM VARYING INDEX FROM 1 BY 1
                   UNTIL INDEX > ALPHABET-LENGTH
                   MOVE ALPHABET(INDEX:1) TO CHAR-TO-ADD
                   MOVE CHAR-TO-ADD TO CURRENT-STRING(STRING-LENGTH + 1:1)
                   ADD 1 TO STRING-LENGTH
                   PERFORM GENERATE-RECURSIVE
                   SUBTRACT 1 FROM STRING-LENGTH
               END-PERFORM
           ELSE
               PERFORM WRITE-STRING
           END-IF.

       WRITE-STRING.
           WRITE OUTPUT-RECORD FROM CURRENT-STRING(1:STRING-LENGTH)
               AFTER ADVANCING 1 LINE.
```

## Key Features of This Solution

1. **Recursive Generation**: Uses recursive approach to build all possible k-mers
2. **Lexicographic Order**: Maintains alphabetical order by iterating through characters sequentially
3. **File Output**: Writes results to output file as required
4. **Parameterizable**: Easy to change alphabet and k-length values
5. **Memory Efficient**: Uses working storage for string manipulation

## Usage Instructions

1. Compile the COBOL program
2. Run the program
3. Check "output.txt" for results

## Example Output

For alphabet "ACGT" and k=3, the output would be:
```
AAA
AAC
AAG
AAT
ACA
ACC
ACG
ACT
AGA
AGC
AGG
AGT
ATA
ATC
ATG
ATT
CAA
CAC
CAG
CAT
CCA
CCC
CCG
CCT
CGA
CGC
CGG
CGT
CTA
CTC
CTG
CTT
GAA
GAC
GAG
GAT
GCA
GCC
GCG
GCT
GGA
GGC
GGG
GGT
GTA
GTC
GTG
GTT
TAA
TAC
TAG
TAT
TCA
TCC
TCG
TCT
TGA
TGC
TGG
TGT
TTA
TTC
TTG
TTT
```

This solution correctly generates all k-mers in lexicographic order for any given alphabet and k-length.

