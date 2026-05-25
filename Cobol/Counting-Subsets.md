# Rosalind Problem: Counting Subsets - COBOL Solution

## Problem Understanding
The problem asks us to count the number of subsets that can be formed from a set of n elements. This is simply 2^n, since each element can either be included or excluded from a subset.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNTING-SUBSETS.
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
       01 INPUT-RECORD.
           05 N-VALUE PIC 9(10).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC 9(20).

       WORKING-STORAGE SECTION.
       01 N PIC 9(10).
       01 RESULT PIC 9(20).
       01 I PIC 9(10).
       01 TEMP PIC 9(20).
       01 MOD-RESULT PIC 9(10).
       01 REMAINDER PIC 9(10).
       01 DONE PIC X VALUE 'N'.
           88 ALL-DONE VALUE 'Y'.
       01 EOF PIC X VALUE 'N'.
           88 END-OF-FILE VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO N-VALUE
               AT END MOVE 'Y' TO EOF
           END-READ

           IF END-OF-FILE
               GO TO END-PROGRAM
           END-IF

           MOVE N-VALUE TO N

           COMPUTE RESULT = 2 ** N

           MOVE RESULT TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       END-PROGRAM.
```

## Alternative Implementation (for larger numbers)

Since the result can be very large (2^n), here's a more robust version that handles modular arithmetic:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNTING-SUBSETS-MOD.
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
       01 INPUT-RECORD.
           05 N-VALUE PIC 9(10).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC 9(20).

       WORKING-STORAGE SECTION.
       01 N PIC 9(10).
       01 RESULT PIC 9(20) VALUE 1.
       01 I PIC 9(10).
       01 MODULUS PIC 9(10) VALUE 1000000.
       01 TEMP PIC 9(20).
       01 EOF PIC X VALUE 'N'.
           88 END-OF-FILE VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO N-VALUE
               AT END MOVE 'Y' TO EOF
           END-READ

           IF END-OF-FILE
               GO TO END-PROGRAM
           END-IF

           MOVE N-VALUE TO N

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               COMPUTE RESULT = (RESULT * 2) MOD MODULUS
           END-PERFORM

           MOVE RESULT TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       END-PROGRAM.
```

## Explanation

1. **First Solution**: Uses the built-in exponentiation operator (`**`) to compute 2^n directly
2. **Second Solution**: Uses modular arithmetic to handle potentially large results, computing 2^n mod 1000000
3. **Input/Output**: Reads from `input.txt` and writes to `output.txt`
4. **File Handling**: Properly opens and closes files
5. **Error Handling**: Checks for end-of-file conditions

## Sample Input/Output

**Input file (input.txt):**
```
3
```

**Output file (output.txt):**
```
8
```

## Notes

- The problem asks for 2^n where n is the number of elements in the set
- For small values of n, the direct computation works fine
- For larger values, modular arithmetic prevents overflow
- The modulo 1000000 is typically used in Rosalind problems to keep results manageable

The solution handles the mathematical concept that a set with n elements has exactly 2^n subsets, including the empty set and the set itself.

