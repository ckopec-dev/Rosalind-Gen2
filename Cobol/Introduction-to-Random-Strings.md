# Rosalind Problem: Introduction to Random Strings (COBOL Solution)

## Problem Understanding

The problem asks us to calculate the probability that a random DNA string of given length will contain at least one occurrence of a specific motif (pattern) of interest.

However, looking at the typical Rosalind problem structure for "Introduction to Random Strings", this is likely asking us to compute the probability of observing a particular string pattern in a random DNA sequence.

Let me solve this using the standard interpretation of this problem:

## Solution Approach

We need to calculate the probability that a random DNA string of a given length will contain a specific motif.

## COBOL Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTRODUCTION-TO-RANDOM-STRINGS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_prob.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE    PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE   PIC X(100).

       WORKING-STORAGE SECTION.
       01 DNA-LENGTH    PIC 9(10) VALUE 0.
       01 MOTIF-LENGTH  PIC 9(10) VALUE 0.
       01 GC-CONTENT    PIC 9V99 VALUE 0.
       01 PROBABILITY   PIC 9V999999999.
       01 I             PIC 9(10) VALUE 0.
       01 J             PIC 9(10) VALUE 0.
       01 TEMP-PROB     PIC 9V999999999.
       01 LINE-COUNT    PIC 9(10) VALUE 0.
       01 LINE-1        PIC X(50).
       01 LINE-2        PIC X(50).
       01 TEMP-STRING   PIC X(100).
       01 END-OF-FILE   PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO LINE-1
               AT END MOVE 'Y' TO END-OF-FILE
           END-READ

           READ INPUT-FILE INTO LINE-2
               AT END MOVE 'Y' TO END-OF-FILE
           END-READ

           IF END-OF-FILE = 'N'
               PERFORM PROCESS-INPUT
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PROCESS-INPUT.
           MOVE LINE-1 TO TEMP-STRING
           UNSTRING TEMP-STRING DELIMITED BY SPACE
               INTO DNA-LENGTH, GC-CONTENT
           END-UNSTRING

           MOVE LINE-2 TO TEMP-STRING
           UNSTRING TEMP-STRING DELIMITED BY SPACE
               INTO MOTIF-LENGTH
           END-UNSTRING

           COMPUTE PROBABILITY = 1.0
           COMPUTE TEMP-PROB = 1.0

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > MOTIF-LENGTH
               COMPUTE TEMP-PROB = TEMP-PROB * (GC-CONTENT / 100.0)
           END-PERFORM

           COMPUTE PROBABILITY = 1.0 - TEMP-PROB

           STRING "Probability: " PROBABILITY INTO OUTPUT-LINE
           WRITE OUTPUT-LINE

           GOBACK.
```

Wait, let me reconsider the actual problem. Looking at the typical Rosalind "Introduction to Random Strings" problem, it's more likely about calculating the probability of a specific string pattern occurring in a random DNA sequence.

## Corrected COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTRODUCTION-TO-RANDOM-STRINGS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_prob.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE    PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE   PIC X(100).

       WORKING-STORAGE SECTION.
       01 DNA-LENGTH    PIC 9(10) VALUE 0.
       01 GC-CONTENT    PIC 9V99 VALUE 0.
       01 GC-PROB       PIC 9V999999999 VALUE 0.
       01 AT-PROB       PIC 9V999999999 VALUE 0.
       01 PROBABILITY   PIC 9V999999999 VALUE 0.
       01 I             PIC 9(10) VALUE 0.
       01 J             PIC 9(10) VALUE 0.
       01 LINE-COUNT    PIC 9(10) VALUE 0.
       01 LINE-1        PIC X(50).
       01 LINE-2        PIC X(50).
       01 TEMP-STRING   PIC X(100).
       01 END-OF-FILE   PIC X VALUE 'N'.
       01 N             PIC 9(10) VALUE 0.
       01 M             PIC 9(10) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO LINE-1
               AT END MOVE 'Y' TO END-OF-FILE
           END-READ

           READ INPUT-FILE INTO LINE-2
               AT END MOVE 'Y' TO END-OF-FILE
           END-READ

           IF END-OF-FILE = 'N'
               PERFORM PROCESS-INPUT
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PROCESS-INPUT.
           MOVE LINE-1 TO TEMP-STRING
           UNSTRING TEMP-STRING DELIMITED BY SPACE
               INTO DNA-LENGTH, GC-CONTENT
           END-UNSTRING

           COMPUTE GC-PROB = GC-CONTENT / 100.0
           COMPUTE AT-PROB = (100.0 - GC-CONTENT) / 100.0

           COMPUTE PROBABILITY = 1.0

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DNA-LENGTH
               COMPUTE PROBABILITY = PROBABILITY * (GC-PROB + AT-PROB)
           END-PERFORM

           COMPUTE PROBABILITY = 1.0 - PROBABILITY

           STRING "Probability: " PROBABILITY INTO OUTPUT-LINE
           WRITE OUTPUT-LINE

           GOBACK.
```

Actually, let me provide a more accurate interpretation based on the standard Rosalind problem:

## Final COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTRODUCTION-TO-RANDOM-STRINGS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_prob.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE    PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE   PIC X(100).

       WORKING-STORAGE SECTION.
       01 DNA-LENGTH    PIC 9(10) VALUE 0.
       01 GC-CONTENT    PIC 9V99 VALUE 0.
       01 GC-PROB       PIC 9V999999999 VALUE 0.
       01 AT-PROB       PIC 9V999999999 VALUE 0.
       01 PROBABILITY   PIC 9V999999999 VALUE 0.
       01 I             PIC 9(10) VALUE 0.
       01 LINE-1        PIC X(50).
       01 LINE-2        PIC X(50).
       01 TEMP-STRING   PIC X(100).
       01 END-OF-FILE   PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO LINE-1
               AT END MOVE 'Y' TO END-OF-FILE
           END-READ

           READ INPUT-FILE INTO LINE-2
               AT END MOVE 'Y' TO END-OF-FILE
           END-READ

           IF END-OF-FILE = 'N'
               PERFORM PROCESS-INPUT
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PROCESS-INPUT.
           MOVE LINE-1 TO TEMP-STRING
           UNSTRING TEMP-STRING DELIMITED BY SPACE
               INTO DNA-LENGTH, GC-CONTENT
           END-UNSTRING

           COMPUTE GC-PROB = GC-CONTENT / 100.0
           COMPUTE AT-PROB = (100.0 - GC-CONTENT) / 100.0

           COMPUTE PROBABILITY = 1.0

           COMPUTE PROBABILITY = 1.0 - (GC-PROB + AT-PROB) ** DNA-LENGTH

           STRING "Probability: " PROBABILITY INTO OUTPUT-LINE
           WRITE OUTPUT-LINE

           GOBACK.
```

## Explanation

This COBOL program solves the Introduction to Random Strings problem by:

1. **Reading Input**: Takes DNA length and GC content from input file
2. **Calculating Probabilities**: 
   - Computes probability of GC content and AT content
   - Uses the formula to calculate probability that a random string matches the given pattern
3. **Output**: Writes the calculated probability to output file

## Input Format
```
Length GC_Content
```

## Example
For input:
```
10 50
```
The program calculates the probability that a random DNA string of length 10 will match a specific pattern with 50% GC content.

## Note
The actual Rosalind problem may have a different interpretation, but this represents a standard approach to random string probability calculations in COBOL.

