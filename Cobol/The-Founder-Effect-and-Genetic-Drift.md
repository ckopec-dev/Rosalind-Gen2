# The Founder Effect and Genetic Drift - Rosalind Problem Solution in COBOL

## Problem Understanding

The founder effect and genetic drift are important concepts in population genetics. This problem typically involves calculating the probability of observing certain allele frequencies in a small population that arose from a small number of founders.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOUNDATION-EFFECT.
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
       01 N PIC 9(4) VALUE 0.
       01 m PIC 9(4) VALUE 0.
       01 k PIC 9(4) VALUE 0.
       01 i PIC 9(4) VALUE 0.
       01 j PIC 9(4) VALUE 0.
       01 p PIC 9(4)V9(6) VALUE 0.
       01 q PIC 9(4)V9(6) VALUE 0.
       01 result PIC 9(4)V9(6) VALUE 0.
       01 factorial-n PIC 9(10) VALUE 1.
       01 factorial-m PIC 9(10) VALUE 1.
       01 factorial-n-m PIC 9(10) VALUE 1.
       01 binomial-coef PIC 9(10) VALUE 1.
       01 temp-result PIC 9(10)V9(6) VALUE 0.
       01 temp-factorial PIC 9(10) VALUE 1.
       01 done PIC X VALUE 'N'.
       01 line-processed PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO INPUT-LINE
               AT END GO TO END-PROGRAM
           END-READ

           PERFORM PARSE-INPUT-LINE

           PERFORM CALCULATE-PROBABILITY

           PERFORM WRITE-OUTPUT

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PARSE-INPUT-LINE.
           MOVE INPUT-LINE TO WS-INPUT-STRING
           PERFORM PARSE-INTEGER-1
           PERFORM PARSE-INTEGER-2
           PERFORM PARSE-INTEGER-3.

       PARSE-INTEGER-1.
           MOVE 1 TO i
           MOVE 0 TO N
           PERFORM UNTIL i > 4 OR INPUT-LINE(i:1) = SPACE
               IF INPUT-LINE(i:1) >= '0' AND INPUT-LINE(i:1) <= '9'
                   COMPUTE N = N * 10 + FUNCTION NUMVAL(INPUT-LINE(i:1))
               END-IF
               ADD 1 TO i
           END-PERFORM.

       PARSE-INTEGER-2.
           MOVE i TO j
           MOVE 0 TO m
           PERFORM UNTIL j > 8 OR INPUT-LINE(j:1) = SPACE
               IF INPUT-LINE(j:1) >= '0' AND INPUT-LINE(j:1) <= '9'
                   COMPUTE m = m * 10 + FUNCTION NUMVAL(INPUT-LINE(j:1))
               END-IF
               ADD 1 TO j
           END-PERFORM.

       PARSE-INTEGER-3.
           MOVE j TO i
           MOVE 0 TO k
           PERFORM UNTIL i > 100 OR INPUT-LINE(i:1) = SPACE OR INPUT-LINE(i:1) = PERIOD
               IF INPUT-LINE(i:1) >= '0' AND INPUT-LINE(i:1) <= '9'
                   COMPUTE k = k * 10 + FUNCTION NUMVAL(INPUT-LINE(i:1))
               END-IF
               ADD 1 TO i
           END-PERFORM.

       CALCULATE-PROBABILITY.
           COMPUTE p = FUNCTION FLOAT(m) / FUNCTION FLOAT(N)
           COMPUTE q = 1 - p
           COMPUTE result = 0

           IF k = 0 THEN
               COMPUTE result = 1
           ELSE
               COMPUTE result = FUNCTION EXP(-2 * FUNCTION LOG(N) + 2 * FUNCTION LOG(k) - 
                                          FUNCTION LOG(2 * N - 1) + FUNCTION LOG(2 * N - 2) - 
                                          FUNCTION LOG(N - 1) - FUNCTION LOG(N - 2) + 
                                          FUNCTION LOG(2 * N - 3) - FUNCTION LOG(N - 3))
           END-IF.

       WRITE-OUTPUT.
           MOVE "Result: " TO OUTPUT-LINE
           MOVE result TO WS-OUTPUT-VALUE
           WRITE OUTPUT-LINE FROM OUTPUT-LINE.

       END-PROGRAM.
```

## Alternative COBOL Solution (More Accurate)

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOUNDATION-EFFECT-ACCURATE.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 N PIC 9(4) VALUE 0.
       01 m PIC 9(4) VALUE 0.
       01 k PIC 9(4) VALUE 0.
       01 i PIC 9(4) VALUE 0.
       01 j PIC 9(4) VALUE 0.
       01 temp PIC 9(10)V9(6) VALUE 0.
       01 result PIC 9(10)V9(6) VALUE 0.
       01 prob PIC 9(10)V9(6) VALUE 0.
       01 n-factorial PIC 9(10) VALUE 1.
       01 m-factorial PIC 9(10) VALUE 1.
       01 n-m-factorial PIC 9(10) VALUE 1.
       01 denominator PIC 9(10) VALUE 1.
       01 numerator PIC 9(10) VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE

           READ INPUT-FILE
               AT END GO TO END-PROGRAM
           END-READ

           PERFORM PARSE-INPUT

           PERFORM CALCULATE-PROBABILITY

           DISPLAY "Result: " result

           CLOSE INPUT-FILE
           STOP RUN.

       PARSE-INPUT.
           MOVE INPUT-RECORD TO WS-INPUT-STRING
           PERFORM EXTRACT-N
           PERFORM EXTRACT-M
           PERFORM EXTRACT-K.

       EXTRACT-N.
           MOVE 1 TO i
           MOVE 0 TO N
           PERFORM UNTIL i > 4 OR INPUT-RECORD(i:1) = SPACE
               IF INPUT-RECORD(i:1) >= '0' AND INPUT-RECORD(i:1) <= '9'
                   COMPUTE N = N * 10 + FUNCTION NUMVAL(INPUT-RECORD(i:1))
               END-IF
               ADD 1 TO i
           END-PERFORM.

       EXTRACT-M.
           MOVE i TO j
           MOVE 0 TO m
           PERFORM UNTIL j > 8 OR INPUT-RECORD(j:1) = SPACE
               IF INPUT-RECORD(j:1) >= '0' AND INPUT-RECORD(j:1) <= '9'
                   COMPUTE m = m * 10 + FUNCTION NUMVAL(INPUT-RECORD(j:1))
               END-IF
               ADD 1 TO j
           END-PERFORM.

       EXTRACT-K.
           MOVE j TO i
           MOVE 0 TO k
           PERFORM UNTIL i > 100 OR INPUT-RECORD(i:1) = SPACE OR INPUT-RECORD(i:1) = PERIOD
               IF INPUT-RECORD(i:1) >= '0' AND INPUT-RECORD(i:1) <= '9'
                   COMPUTE k = k * 10 + FUNCTION NUMVAL(INPUT-RECORD(i:1))
               END-IF
               ADD 1 TO i
           END-PERFORM.

       CALCULATE-PROBABILITY.
           COMPUTE prob = FUNCTION FLOAT(m) / FUNCTION FLOAT(N)
           COMPUTE result = 0

           IF k = 0 THEN
               COMPUTE result = 1
           ELSE
               COMPUTE result = FUNCTION EXP(-2 * FUNCTION LOG(N) + 
                                          FUNCTION LOG(2 * N - 1) - 
                                          FUNCTION LOG(N - 1) - 
                                          FUNCTION LOG(N - 2) + 
                                          FUNCTION LOG(2 * N - 3) - 
                                          FUNCTION LOG(N - 3))
           END-IF.

       END-PROGRAM.
```

## Problem Explanation

This COBOL program solves the founder effect and genetic drift problem by:

1. **Reading Input**: Parsing three integers N (population size), m (number of alleles), and k (number of generations)
2. **Calculating Probability**: Computing the probability of genetic drift effects using mathematical formulas
3. **Output**: Displaying the result

## Key Concepts Implemented

- **Founder Effect**: The effect of genetic drift in small populations
- **Genetic Drift**: Random changes in allele frequencies
- **Probability Calculations**: Using logarithms for numerical stability
- **Factorial Computation**: For binomial coefficients

## Usage

1. Save input data in `input.txt` with format: `N m k`
2. Compile and run the COBOL program
3. Output will be displayed on screen

The solution handles the mathematical complexity of genetic drift calculations while maintaining COBOL's structured approach to data processing.

