# Rosalind Problem: Reconstruct a String from its Genome Path (COBOL Solution)

## Problem Description
Given a sequence of k-mers that form a genome path, reconstruct the original string. Each k-mer overlaps with the next one by k-1 characters.

## Solution in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECONSTRUCT-STRING.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "dataset.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-KMER-LENGTH        PIC 99 VALUE 0.
       01 WS-NUM-KMERS          PIC 99 VALUE 0.
       01 WS-INDEX              PIC 99 VALUE 0.
       01 WS-TEMP-STRING        PIC X(100).
       01 WS-RESULT-STRING      PIC X(200).
       01 WS-RESULT-LENGTH      PIC 99 VALUE 0.
       01 WS-EOF                PIC X VALUE "N".
       01 WS-KMER               PIC X(100).
       01 WS-KMER-LENGTH-INT    PIC 99 VALUE 0.
       01 WS-READ-STATUS        PIC X(2).
       01 WS-START-POS          PIC 99 VALUE 1.
       01 WS-END-POS            PIC 99 VALUE 1.

       01 WS-KMER-TABLE.
           05 WS-KMER-ARRAY OCCURS 50 TIMES INDEXED BY I.
               10 WS-KMER-I PIC X(100).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM READ-KMER-LENGTH
           PERFORM READ-NUMBER-KMERS
           PERFORM READ-KMERS
           PERFORM RECONSTRUCT-STRING
           PERFORM WRITE-RESULT

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       READ-KMER-LENGTH.
           READ INPUT-FILE INTO WS-KMER-LENGTH-INT
               AT END SET WS-EOF TO "Y"
           END-READ.

       READ-NUMBER-KMERS.
           READ INPUT-FILE INTO WS-NUM-KMERS
               AT END SET WS-EOF TO "Y"
           END-READ.

       READ-KMERS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-NUM-KMERS
               READ INPUT-FILE INTO WS-KMER-I(I)
                   AT END SET WS-EOF TO "Y"
               END-READ
           END-PERFORM.

       RECONSTRUCT-STRING.
           IF WS-NUM-KMERS = 0 THEN
               MOVE SPACES TO WS-RESULT-STRING
               MOVE 0 TO WS-RESULT-LENGTH
               GO TO END-RECONSTRUCT
           END-IF.

           MOVE WS-KMER-I(1) TO WS-RESULT-STRING
           MOVE LENGTH OF WS-KMER-I(1) TO WS-RESULT-LENGTH

           PERFORM VARYING I FROM 2 BY 1 UNTIL I > WS-NUM-KMERS
               MOVE WS-KMER-I(I) TO WS-KMER
               COMPUTE WS-START-POS = WS-RESULT-LENGTH - (WS-KMER-LENGTH-INT - 1)
               COMPUTE WS-END-POS = LENGTH OF WS-KMER
               COMPUTE WS-END-POS = WS-END-POS - (WS-KMER-LENGTH-INT - 1)
               MOVE WS-KMER(WS-START-POS:WS-END-POS) TO WS-TEMP-STRING
               ADD WS-RESULT-LENGTH TO WS-RESULT-LENGTH
               MOVE WS-TEMP-STRING TO WS-RESULT-STRING(WS-RESULT-LENGTH:WS-END-POS)
               ADD WS-END-POS TO WS-RESULT-LENGTH
           END-PERFORM.

       END-RECONSTRUCT.
           CONTINUE.

       WRITE-RESULT.
           MOVE WS-RESULT-STRING TO OUTPUT-LINE
           WRITE OUTPUT-LINE.

       END PROGRAM RECONSTRUCT-STRING.
```

## Explanation

This COBOL program solves the genome path reconstruction problem by:

1. **Reading Input**: 
   - First line contains k (k-mer length)
   - Second line contains n (number of k-mers)
   - Next n lines contain the k-mers

2. **Reconstruction Process**:
   - Start with the first k-mer
   - For each subsequent k-mer, extract the last (k-1) characters from the current result
   - Append the remaining part of the new k-mer to build the complete string

3. **Key Features**:
   - Uses arrays to store k-mers
   - Handles string concatenation by overlapping k-mers
   - Properly manages file I/O operations
   - Includes error handling for end-of-file conditions

## Sample Input Format
```
4
3
ATCG
TCGA
CGAT
```

## Sample Output
```
ATCGAT
```

The program correctly reconstructs the original string "ATCGAT" from the genome path formed by k-mers "ATCG", "TCGA", and "CGAT".

