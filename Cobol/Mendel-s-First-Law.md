# Rosalind Problem: Mendel's First Law (Hardy-Weinberg Equilibrium) in COBOL

## Problem Understanding

The problem asks us to calculate the probability that two randomly selected organisms will produce an offspring with a dominant phenotype, given the counts of homozygous dominant (k), heterozygous (m), and homozygous recessive (n) organisms in a population.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENDELS-FIRST-LAW.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_iprb.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE PIC X(50).

       WORKING-STORAGE SECTION.
       01 K PIC 9(5) VALUE 0.
       01 M PIC 9(5) VALUE 0.
       01 N PIC 9(5) VALUE 0.
       01 TOTAL PIC 9(6) VALUE 0.
       01 PROBABILITY PIC 9.999999 VALUE 0.
       01 TEMP1 PIC 9.999999 VALUE 0.
       01 TEMP2 PIC 9.999999 VALUE 0.
       01 TEMP3 PIC 9.999999 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO INPUT-LINE
               AT END GO TO END-PROGRAM
           END-READ

           PERFORM PARSE-INPUT
           PERFORM CALCULATE-PROBABILITY
           PERFORM WRITE-RESULT

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PARSE-INPUT.
           MOVE INPUT-LINE TO WS-INPUT-TEMP.
           STRING K, M, N DELIMITED BY SIZE
               INTO WS-INPUT-TEMP
           END-STRING.

           MOVE 1 TO WS-START-POS.
           PERFORM UNTIL WS-START-POS > 100
               MOVE 0 TO WS-TEMP-VALUE
               PERFORM EXTRACT-NUMBER
               IF WS-TEMP-VALUE NOT = 0
                   IF K = 0
                       MOVE WS-TEMP-VALUE TO K
                   ELSE IF M = 0
                       MOVE WS-TEMP-VALUE TO M
                   ELSE IF N = 0
                       MOVE WS-TEMP-VALUE TO N
                   END-IF
               END-IF
               ADD 1 TO WS-START-POS
               IF WS-START-POS > 100
                   GO TO PARSE-END
               END-IF
           END-PERFORM.

       PARSE-END.
           EXIT.

       EXTRACT-NUMBER.
           MOVE 0 TO WS-TEMP-VALUE.
           MOVE 1 TO WS-INDEX.
           PERFORM UNTIL WS-INDEX > 5 OR 
                   INPUT-LINE(WS-START-POS:1) = SPACE OR
                   INPUT-LINE(WS-START-POS:1) = X'0D' OR
                   INPUT-LINE(WS-START-POS:1) = X'0A'
               IF INPUT-LINE(WS-START-POS:1) >= '0' AND
                  INPUT-LINE(WS-START-POS:1) <= '9'
                   COMPUTE WS-TEMP-VALUE = WS-TEMP-VALUE * 10 + 
                       (INPUT-LINE(WS-START-POS:1) - '0')
               END-IF
               ADD 1 TO WS-START-POS
               ADD 1 TO WS-INDEX
           END-PERFORM.

       CALCULATE-PROBABILITY.
           COMPUTE TOTAL = K + M + N

           IF TOTAL = 0
               GO TO WRITE-RESULT
           END-IF

           COMPUTE TEMP1 = (K * (K - 1)) / (TOTAL * (TOTAL - 1))
           COMPUTE TEMP2 = (K * M) / (TOTAL * (TOTAL - 1))
           COMPUTE TEMP3 = (M * (M - 1)) / (TOTAL * (TOTAL - 1))

           COMPUTE PROBABILITY = 
               (TEMP1 * 1.0) +           (* All dominant *)
               (TEMP2 * 1.0) +           (* One dominant, one hetero *)
               (TEMP3 * 0.75) +          (* Two hetero -> 3/4 dominant *)
               (K * N) / (TOTAL * (TOTAL - 1)) +  (* One dominant, one recessive *)
               (M * N) / (TOTAL * (TOTAL - 1))    (* One hetero, one recessive *)

           COMPUTE PROBABILITY = 
               (TEMP1 * 1.0) + 
               (TEMP2 * 1.0) + 
               (TEMP3 * 0.75) + 
               (K * N) / (TOTAL * (TOTAL - 1)) + 
               (M * N) / (TOTAL * (TOTAL - 1))

           COMPUTE PROBABILITY = 
               (K * (K - 1)) / (TOTAL * (TOTAL - 1)) * 1.0 +
               (2 * K * M) / (TOTAL * (TOTAL - 1)) * 1.0 +
               (M * (M - 1)) / (TOTAL * (TOTAL - 1)) * 0.75 +
               (2 * K * N) / (TOTAL * (TOTAL - 1)) * 1.0 +
               (2 * M * N) / (TOTAL * (TOTAL - 1)) * 0.5

           COMPUTE PROBABILITY = 
               (K * (K - 1)) / (TOTAL * (TOTAL - 1)) +
               (2 * K * M) / (TOTAL * (TOTAL - 1)) +
               (M * (M - 1)) / (TOTAL * (TOTAL - 1)) * 0.75 +
               (2 * K * N) / (TOTAL * (TOTAL - 1)) +
               (2 * M * N) / (TOTAL * (TOTAL - 1)) * 0.5.

       WRITE-RESULT.
           MOVE PROBABILITY TO OUTPUT-LINE
           WRITE OUTPUT-LINE
           DISPLAY PROBABILITY.

       END-PROGRAM MENDELS-FIRST-LAW.
```

## Alternative Cleaner Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENDELS-FIRST-LAW.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_iprb.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(50).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE PIC 9.999999.

       WORKING-STORAGE SECTION.
       01 K PIC 9(5) VALUE 0.
       01 M PIC 9(5) VALUE 0.
       01 N PIC 9(5) VALUE 0.
       01 TOTAL PIC 9(6) VALUE 0.
       01 PROBABILITY PIC 9.999999 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO INPUT-LINE
               AT END GO TO END-PROGRAM
           END-READ

           PERFORM PARSE-INPUT
           PERFORM CALCULATE-PROBABILITY
           PERFORM WRITE-RESULT

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PARSE-INPUT.
           MOVE INPUT-LINE TO WS-INPUT-TEMP.
           UNSTRING INPUT-LINE DELIMITED BY SPACE
               INTO K, M, N
           END-UNSTRING.

       CALCULATE-PROBABILITY.
           COMPUTE TOTAL = K + M + N
           COMPUTE PROBABILITY = 
               (K * (K - 1) + 2 * K * M + 0.75 * M * (M - 1) + 
                2 * K * N + M * N) / (TOTAL * (TOTAL - 1)).

       WRITE-RESULT.
           WRITE OUTPUT-LINE FROM PROBABILITY
           DISPLAY PROBABILITY.

       END-PROGRAM MENDELS-FIRST-LAW.
```

## Explanation

The COBOL solution follows these steps:

1. **Input Processing**: Read the input file containing three integers k, m, n
2. **Parse Input**: Extract the three values from the input line
3. **Calculate Probability**: Apply Mendel's first law using the formula:
   - P(dominant phenotype) = 1 - P(recessive phenotype)
   - P(recessive) = (n/N) × ((n-1)/(N-1)) for two recessive individuals
   - Or more directly: P(dominant) = 1 - (n/N) × ((n-1)/(N-1)) for the case of two recessive parents

4. **Output**: Write the calculated probability to output file

The key insight is that we need to calculate the probability that two randomly selected organisms will produce a dominant offspring, which involves considering all possible mating pairs and their probabilities.

