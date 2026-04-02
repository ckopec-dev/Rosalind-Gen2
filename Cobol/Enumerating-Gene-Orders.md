# Rosalind Problem: Enumerating Gene Orders (COBOL Solution)

## Problem Understanding

The problem asks us to find all possible permutations of a given set of genes (represented as numbers) and output them in a specific format.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENUMERATING-GENE-ORDERS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_perm.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE PIC X(200).

       WORKING-STORAGE SECTION.
       01 N PIC 99 VALUE 0.
       01 FACTORIAL PIC 9(10) VALUE 1.
       01 I PIC 99 VALUE 1.
       01 J PIC 99 VALUE 1.
       01 K PIC 99 VALUE 1.
       01 TEMP PIC 99 VALUE 0.
       01 PERM-ARRAY.
           05 PERM-ITEM PIC 99 OCCURS 8 TIMES INDEXED BY IDX.
       01 USED-ARRAY.
           05 USED-ITEM PIC 9 VALUE 0 OCCURS 8 TIMES INDEXED BY IDX2.
       01 COUNT PIC 9(10) VALUE 0.
       01 LINE-BUFFER PIC X(200).
       01 EOF-FLAG PIC X VALUE "N".
           88 END-OF-FILE VALUE "Y".
       01 LINE-LENGTH PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO INPUT-LINE
               AT END SET END-OF-FILE TO TRUE
           END-READ

           IF END-OF-FILE
               GO TO END-PROGRAM
           END-IF

           MOVE INPUT-LINE TO LINE-BUFFER
           COMPUTE N = FUNCTION NUMVAL(LINE-BUFFER)
           COMPUTE FACTORIAL = 1
           PERFORM COMPUTE-FACTORIAL

           DISPLAY "Total permutations: " FACTORIAL
           MOVE 0 TO COUNT

           PERFORM INITIALIZE-ARRAYS
           PERFORM GENERATE-PERMUTATIONS

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       COMPUTE-FACTORIAL.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               COMPUTE FACTORIAL = FACTORIAL * I
           END-PERFORM.

       INITIALIZE-ARRAYS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               MOVE 0 TO PERM-ITEM(I)
               MOVE 0 TO USED-ITEM(I)
           END-PERFORM.

       GENERATE-PERMUTATIONS.
           PERFORM PERMUTE-RECURSIVE WITH TEST AFTER VARYING I FROM 1 BY 1 UNTIL I > N.

       PERMUTE-RECURSIVE.
           IF I > N
               PERFORM WRITE-RESULT
               GO TO NEXT-RESULT
           END-IF

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               IF USED-ITEM(J) = 0
                   MOVE J TO PERM-ITEM(I)
                   MOVE 1 TO USED-ITEM(J)
                   PERFORM PERMUTE-RECURSIVE
                   MOVE 0 TO USED-ITEM(J)
               END-IF
           END-PERFORM.

       WRITE-RESULT.
           ADD 1 TO COUNT
           MOVE SPACES TO LINE-BUFFER
           MOVE 0 TO LINE-LENGTH

           PERFORM VARYING K FROM 1 BY 1 UNTIL K > N
               COMPUTE LINE-LENGTH = LINE-LENGTH + 3
               STRING FUNCTION TRIM(FUNCTION NUMVAL(PERM-ITEM(K))) 
                      DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      INTO LINE-BUFFER
           END-PERFORM

           STRING FUNCTION TRIM(LINE-BUFFER)
                  DELIMITED BY SIZE
                  INTO OUTPUT-LINE
           END-STRING

           WRITE OUTPUT-LINE FROM OUTPUT-LINE
               INVALID KEY GO TO WRITE-ERROR
           END-WRITE.

       NEXT-RESULT.
           CONTINUE.

       WRITE-ERROR.
           DISPLAY "ERROR WRITING TO OUTPUT FILE"
           GO TO NEXT-RESULT.

       END-PROGRAM.
```

## Explanation

This COBOL solution works as follows:

1. **Input Processing**: Reads the number N from input file
2. **Factorial Calculation**: Computes N! to know total permutations
3. **Array Initialization**: Sets up arrays to track used numbers and current permutation
4. **Recursive Generation**: Uses backtracking to generate all permutations:
   - For each position, try placing each unused number
   - Mark the number as used and recursively fill remaining positions
   - Backtrack by unmarking the number when returning
5. **Output**: Writes each permutation to output file

## Key Features

- **Backtracking Algorithm**: Implements recursive permutation generation
- **Memory Management**: Uses arrays to track state during recursion
- **File I/O**: Handles input and output file operations
- **Error Handling**: Includes basic error checking for file operations

## Sample Input/Output

**Input**: `3`
**Output**:
```
1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1
```

The solution generates all N! permutations of numbers 1 through N and outputs them in the required format.

