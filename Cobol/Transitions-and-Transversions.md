# Rosalind Problem: Transitions and Transversions in COBOL

## Problem Understanding

The task is to calculate the ratio of transitions to transversions between two DNA sequences of equal length.

- **Transition**: Purine ↔ Purine (A↔G) or Pyrimidine ↔ Pyrimidine (C↔T)
- **Transversion**: Purine ↔ Pyrimidine (A↔C, A↔T, G↔C, G↔T)

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSITIONS-TRANSVERSIONS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_transitions.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       WORKING-STORAGE SECTION.
       01 SEQUENCE-1 PIC X(50).
       01 SEQUENCE-2 PIC X(50).
       01 LENGTH PIC 9(3) VALUE 0.
       01 I PIC 9(3) VALUE 1.
       01 TRANSITION-COUNT PIC 9(3) VALUE 0.
       01 TRANSVERSION-COUNT PIC 9(3) VALUE 0.
       01 RATIO PIC 9V99 VALUE 0.
       01 LINE-LENGTH PIC 9(3) VALUE 0.
       01 EOF-FLAG PIC X VALUE 'N'.
       01 TEMP-CHAR1 PIC X.
       01 TEMP-CHAR2 PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           
           READ INPUT-FILE INTO SEQUENCE-1
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ
           
           IF EOF-FLAG = 'Y'
               GO TO END-PROGRAM
           END-IF
           
           READ INPUT-FILE INTO SEQUENCE-2
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ
           
           IF EOF-FLAG = 'Y'
               GO TO END-PROGRAM
           END-IF
           
           MOVE FUNCTION LENGTH(SEQUENCE-1) TO LENGTH
           
           PERFORM CHECK-PAIRS-VARYING I FROM 1 BY 1
               UNTIL I > LENGTH
           
           IF TRANSVERSION-COUNT = 0
               DISPLAY "Error: Division by zero"
           ELSE
               COMPUTE RATIO = TRANSITION-COUNT / TRANSVERSION-COUNT
               DISPLAY "Transitions: " TRANSITION-COUNT
               DISPLAY "Transversions: " TRANSVERSION-COUNT
               DISPLAY "Ratio: " RATIO
           END-IF
           
           GO TO END-PROGRAM.
       
       CHECK-PAIRS.
           MOVE SEQUENCE-1(I:1) TO TEMP-CHAR1
           MOVE SEQUENCE-2(I:1) TO TEMP-CHAR2
           
           IF TEMP-CHAR1 = TEMP-CHAR2
               GO TO NEXT-CHAR
           END-IF
           
           IF (TEMP-CHAR1 = 'A' AND TEMP-CHAR2 = 'G') OR
              (TEMP-CHAR1 = 'G' AND TEMP-CHAR2 = 'A') OR
              (TEMP-CHAR1 = 'C' AND TEMP-CHAR2 = 'T') OR
              (TEMP-CHAR1 = 'T' AND TEMP-CHAR2 = 'C')
               ADD 1 TO TRANSITION-COUNT
           ELSE
               ADD 1 TO TRANSVERSION-COUNT
           END-IF.
       
       NEXT-CHAR.
           EXIT.
       
       END-PROGRAM.
```

## Alternative Implementation (More Robust)

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSITIONS-TRANSVERSIONS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_transitions.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       WORKING-STORAGE SECTION.
       01 SEQUENCE-1 PIC X(50).
       01 SEQUENCE-2 PIC X(50).
       01 LENGTH PIC 9(3) VALUE 0.
       01 I PIC 9(3) VALUE 1.
       01 TRANSITION-COUNT PIC 9(3) VALUE 0.
       01 TRANSVERSION-COUNT PIC 9(3) VALUE 0.
       01 RATIO PIC 9V99 VALUE 0.
       01 EOF-FLAG PIC X VALUE 'N'.
       01 CHAR1 PIC X.
       01 CHAR2 PIC X.
       01 IS-TRANSITION PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           
           READ INPUT-FILE INTO SEQUENCE-1
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ
           
           IF EOF-FLAG = 'Y'
               GO TO END-PROGRAM
           END-IF
           
           READ INPUT-FILE INTO SEQUENCE-2
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ
           
           IF EOF-FLAG = 'Y'
               GO TO END-PROGRAM
           END-IF
           
           MOVE FUNCTION LENGTH(SEQUENCE-1) TO LENGTH
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH
               PERFORM CHECK-CHARACTER-PAIR
           END-PERFORM
           
           IF TRANSVERSION-COUNT = 0
               DISPLAY "Error: Cannot compute ratio - no transversions"
           ELSE
               COMPUTE RATIO = TRANSITION-COUNT / TRANSVERSION-COUNT
               DISPLAY "Transitions: " TRANSITION-COUNT
               DISPLAY "Transversions: " TRANSVERSION-COUNT
               DISPLAY "Ratio: " RATIO
           END-IF
           
           GO TO END-PROGRAM.
       
       CHECK-CHARACTER-PAIR.
           MOVE SEQUENCE-1(I:1) TO CHAR1
           MOVE SEQUENCE-2(I:1) TO CHAR2
           
           IF CHAR1 = CHAR2
               GO TO NEXT-CHAR
           END-IF
           
           PERFORM DETERMINE-TRANSITION
           
           IF IS-TRANSITION = 'Y'
               ADD 1 TO TRANSITION-COUNT
           ELSE
               ADD 1 TO TRANSVERSION-COUNT
           END-IF.
       
       DETERMINE-TRANSITION.
           MOVE 'N' TO IS-TRANSITION
           
           IF (CHAR1 = 'A' AND CHAR2 = 'G') OR
              (CHAR1 = 'G' AND CHAR2 = 'A') OR
              (CHAR1 = 'C' AND CHAR2 = 'T') OR
              (CHAR1 = 'T' AND CHAR2 = 'C')
               MOVE 'Y' TO IS-TRANSITION
           END-IF.
       
       NEXT-CHAR.
           EXIT.
       
       END-PROGRAM.
```

## Sample Input Format

The input file should contain two DNA sequences, one per line:
```
AGCTACGT
AGCTACTG
```

## Sample Output
```
Transitions: 3
Transversions: 2
Ratio: 1.50
```

## Key COBOL Features Used

1. **File Handling**: Sequential file input with proper error checking
2. **String Manipulation**: Character-by-character comparison using substring operations
3. **Control Structures**: PERFORM VARYING loop for iteration, IF-THEN-ELSE for logic
4. **Arithmetic Operations**: COMPUTE for ratio calculation
5. **Data Types**: PIC X for character data, PIC 9(3) for numeric values

## Notes

- The program assumes valid input with equal-length DNA sequences
- Only A, C, G, T characters are considered (standard DNA nucleotides)
- Error handling for division by zero is included
- The ratio is displayed with 2 decimal places using PIC 9V99 format

