# Rosalind Problem: Implement_GreedyMotifSearch_with_Pseudocounts in COBOL

## Problem Understanding

This problem requires implementing a greedy motif search algorithm with pseudocounts to find the best motif in a set of DNA sequences. The algorithm should:
1. Start with the first k-mer from the first sequence
2. Iteratively build a profile matrix
3. Use pseudocounts to avoid zero probabilities
4. Find the most probable k-mer in each subsequent sequence
5. Return the best collection of k-mers

## COBOL Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GREEDYMOTIFSEARCH.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
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
       01 N PIC 99 VALUE 0.
       01 K PIC 99 VALUE 0.
       01 T PIC 99 VALUE 0.
       01 I PIC 99 VALUE 0.
       01 J PIC 99 VALUE 0.
       01 L PIC 99 VALUE 0.
       01 M PIC 99 VALUE 0.
       01 P PIC 99 VALUE 0.
       01 Q PIC 99 VALUE 0.
       01 MAX-VALUE PIC 9V999999 VALUE 0.
       01 TEMP-VALUE PIC 9V999999 VALUE 0.
       01 BEST-SCORE PIC 9999 VALUE 0.
       01 CURRENT-SCORE PIC 9999 VALUE 0.

       01 DNA-SEQUENCES.
           05 SEQUENCE-1 PIC X(50).
           05 SEQUENCE-2 PIC X(50).
           05 SEQUENCE-3 PIC X(50).
           05 SEQUENCE-4 PIC X(50).
           05 SEQUENCE-5 PIC X(50).
           05 SEQUENCE-6 PIC X(50).
           05 SEQUENCE-7 PIC X(50).
           05 SEQUENCE-8 PIC X(50).
           05 SEQUENCE-9 PIC X(50).
           05 SEQUENCE-10 PIC X(50).

       01 MOTIFS.
           05 MOTIF-1 PIC X(10).
           05 MOTIF-2 PIC X(10).
           05 MOTIF-3 PIC X(10).
           05 MOTIF-4 PIC X(10).
           05 MOTIF-5 PIC X(10).
           05 MOTIF-6 PIC X(10).
           05 MOTIF-7 PIC X(10).
           05 MOTIF-8 PIC X(10).
           05 MOTIF-9 PIC X(10).
           05 MOTIF-10 PIC X(10).

       01 PROFILE-MATRIX.
           05 PROFILE-A OCCURS 10 TIMES PIC 9V999999.
           05 PROFILE-C OCCURS 10 TIMES PIC 9V999999.
           05 PROFILE-G OCCURS 10 TIMES PIC 9V999999.
           05 PROFILE-T OCCURS 10 TIMES PIC 9V999999.

       01 COUNT-MATRIX.
           05 COUNT-A OCCURS 10 TIMES PIC 99.
           05 COUNT-C OCCURS 10 TIMES PIC 99.
           05 COUNT-G OCCURS 10 TIMES PIC 99.
           05 COUNT-T OCCURS 10 TIMES PIC 99.

       01 ALPHABET PIC X(4) VALUE "ACGT".
       01 ALPHABET-INDEX PIC 99 VALUE 0.

       01 TEMP-MOTIF PIC X(10).
       01 BEST-MOTIF PIC X(10).
       01 CURRENT-MOTIF PIC X(10).
       01 TEMP-CHAR PIC X(1).
       01 BEST-SCORE-STRING PIC X(100).
       01 TEMP-STRING PIC X(100).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM READ-INPUT.
           PERFORM GREEDY-MOTIF-SEARCH.
           PERFORM WRITE-OUTPUT.
           STOP RUN.

       READ-INPUT.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO TEMP-STRING.
           UNSTRING TEMP-STRING DELIMITED BY SPACE
               INTO N, K, T.
           MOVE 0 TO I.
           PERFORM UNTIL I = T
               ADD 1 TO I
               READ INPUT-FILE INTO 
                   FUNCTION SUBSTRING(SEQUENCE-I, 1, 50)
           END-PERFORM.
           CLOSE INPUT-FILE.

       GREEDY-MOTIF-SEARCH.
           MOVE 0 TO BEST-SCORE.
           MOVE 0 TO I.
           PERFORM UNTIL I = N - K + 1
               ADD 1 TO I
               MOVE FUNCTION SUBSTRING(SEQUENCE-1, I, K) TO TEMP-MOTIF
               PERFORM INITIALIZE-PROFILE.
               PERFORM BUILD-PROFILE.
               PERFORM FIND-BEST-MOTIF.
               PERFORM CALCULATE-SCORE.
               IF CURRENT-SCORE > BEST-SCORE
                   MOVE CURRENT-SCORE TO BEST-SCORE
                   MOVE TEMP-MOTIF TO BEST-MOTIF
               END-IF
           END-PERFORM.

       INITIALIZE-PROFILE.
           PERFORM INITIALIZE-COUNTS.
           PERFORM INITIALIZE-PROFILE-MATRIX.

       INITIALIZE-COUNTS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > K
               MOVE 0 TO COUNT-A(I)
               MOVE 0 TO COUNT-C(I)
               MOVE 0 TO COUNT-G(I)
               MOVE 0 TO COUNT-T(I)
           END-PERFORM.

       INITIALIZE-PROFILE-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > K
               MOVE 0.0 TO PROFILE-A(I)
               MOVE 0.0 TO PROFILE-C(I)
               MOVE 0.0 TO PROFILE-G(I)
               MOVE 0.0 TO PROFILE-T(I)
           END-PERFORM.

       BUILD-PROFILE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > T
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > K
                   MOVE FUNCTION SUBSTRING(SEQUENCE-I, J, 1) TO TEMP-CHAR
                   IF TEMP-CHAR = "A"
                       ADD 1 TO COUNT-A(J)
                   ELSE IF TEMP-CHAR = "C"
                       ADD 1 TO COUNT-C(J)
                   ELSE IF TEMP-CHAR = "G"
                       ADD 1 TO COUNT-G(J)
                   ELSE IF TEMP-CHAR = "T"
                       ADD 1 TO COUNT-T(J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       CALCULATE-PROFILE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > K
               COMPUTE PROFILE-A(J) = (COUNT-A(J) + 1) / (T + 4)
               COMPUTE PROFILE-C(J) = (COUNT-C(J) + 1) / (T + 4)
               COMPUTE PROFILE-G(J) = (COUNT-G(J) + 1) / (T + 4)
               COMPUTE PROFILE-T(J) = (COUNT-T(J) + 1) / (T + 4)
           END-PERFORM.

       FIND-BEST-MOTIF.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > T
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > N - K + 1
                   MOVE FUNCTION SUBSTRING(SEQUENCE-I, J, K) TO CURRENT-MOTIF
                   PERFORM CALCULATE-MOTIF-PROBABILITY.
                   IF TEMP-VALUE > MAX-VALUE
                       MOVE TEMP-VALUE TO MAX-VALUE
                       MOVE CURRENT-MOTIF TO BEST-MOTIF
                   END-IF
               END-PERFORM
           END-PERFORM.

       CALCULATE-MOTIF-PROBABILITY.
           MOVE 1.0 TO TEMP-VALUE.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > K
               MOVE FUNCTION SUBSTRING(CURRENT-MOTIF, J, 1) TO TEMP-CHAR
               IF TEMP-CHAR = "A"
                   COMPUTE TEMP-VALUE = TEMP-VALUE * PROFILE-A(J)
               ELSE IF TEMP-CHAR = "C"
                   COMPUTE TEMP-VALUE = TEMP-VALUE * PROFILE-C(J)
               ELSE IF TEMP-CHAR = "G"
                   COMPUTE TEMP-VALUE = TEMP-VALUE * PROFILE-G(J)
               ELSE IF TEMP-CHAR = "T"
                   COMPUTE TEMP-VALUE = TEMP-VALUE * PROFILE-T(J)
               END-IF
           END-PERFORM.

       CALCULATE-SCORE.
           MOVE 0 TO CURRENT-SCORE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > T
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > K
                   MOVE FUNCTION SUBSTRING(SEQUENCE-I, J, 1) TO TEMP-CHAR
                   IF TEMP-CHAR = "A"
                       ADD 1 TO CURRENT-SCORE
                   ELSE IF TEMP-CHAR = "C"
                       ADD 1 TO CURRENT-SCORE
                   ELSE IF TEMP-CHAR = "G"
                       ADD 1 TO CURRENT-SCORE
                   ELSE IF TEMP-CHAR = "T"
                       ADD 1 TO CURRENT-SCORE
                   END-IF
               END-PERFORM
           END-PERFORM.

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE.
           MOVE BEST-MOTIF TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.
           CLOSE OUTPUT-FILE.

       END PROGRAM GREEDYMOTIFSEARCH.
```

## Explanation of Key Components

### 1. **Data Structures**
- `DNA-SEQUENCES`: Array to store input DNA sequences
- `MOTIFS`: Array to store motifs found in each sequence
- `PROFILE-MATRIX`: Matrix to store probability values for each position
- `COUNT-MATRIX`: Matrix to store nucleotide counts

### 2. **Key Algorithms**
- **Greedy Motif Search**: Starts with first k-mer from first sequence
- **Pseudocounts**: Adds 1 to each count to avoid zero probabilities
- **Profile Construction**: Converts counts to probabilities using pseudocounts
- **Motif Selection**: Finds most probable k-mer in each sequence

### 3. **Pseudocount Implementation**
The key innovation is in the profile calculation:
```
PROFILE-A(J) = (COUNT-A(J) + 1) / (T + 4)
```
This ensures no zero probabilities occur, which would cause issues in the motif probability calculations.

### 4. **Algorithm Steps**
1. Read input parameters (N, K, T) and sequences
2. For each possible k-mer in the first sequence:
   - Initialize profile with pseudocounts
   - Build profile matrix from current motifs
   - Find best motif in subsequent sequences
   - Calculate score and track best solution
3. Output the best motif found

This implementation handles the core requirements of the greedy motif search with pseudocounts while maintaining COBOL's structured approach to programming.

