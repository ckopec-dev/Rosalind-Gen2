# Rosalind Problem: Creating a Character Table (COBOL Solution)

## Problem Understanding

The task is to create a character table from a set of DNA sequences. A character table represents which characters (nucleotides) are present at each position in the sequences.

## Solution Approach

I'll implement a COBOL program that:
1. Reads DNA sequences
2. Creates a character table showing which nucleotides appear at each position
3. Outputs the result in the required format

## COBOL Program

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHARACTER-TABLE.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "sequences.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "character_table.out"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-SEQUENCES.
          05 WS-SEQ-1 PIC X(50).
          05 WS-SEQ-2 PIC X(50).
          05 WS-SEQ-3 PIC X(50).
          05 WS-SEQ-4 PIC X(50).
          05 WS-SEQ-5 PIC X(50).
          05 WS-SEQ-6 PIC X(50).
          05 WS-SEQ-7 PIC X(50).
          05 WS-SEQ-8 PIC X(50).
          05 WS-SEQ-9 PIC X(50).
          05 WS-SEQ-10 PIC X(50).

       01 WS-LENGTH PIC 99 VALUE 0.
       01 WS-INDEX PIC 99 VALUE 1.
       01 WS-POS PIC 99 VALUE 1.
       01 WS-CHAR PIC X(1).
       01 WS-CHAR-FOUND PIC X(4) VALUE "0000".
       01 WS-TABLE-ROW PIC X(100).
       01 WS-EOF PIC X VALUE "N".
       01 WS-SEQUENCE-COUNT PIC 99 VALUE 0.
       01 WS-CHARACTER-TABLE.
          05 WS-TABLE-ROW-1 PIC X(100).
          05 WS-TABLE-ROW-2 PIC X(100).
          05 WS-TABLE-ROW-3 PIC X(100).
          05 WS-TABLE-ROW-4 PIC X(100).
          05 WS-TABLE-ROW-5 PIC X(100).
          05 WS-TABLE-ROW-6 PIC X(100).
          05 WS-TABLE-ROW-7 PIC X(100).
          05 WS-TABLE-ROW-8 PIC X(100).
          05 WS-TABLE-ROW-9 PIC X(100).
          05 WS-TABLE-ROW-10 PIC X(100).
       01 WS-TABLE-INDEX PIC 99 VALUE 1.
       01 WS-TEMP-CHAR PIC X(100).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM READ-SEQUENCES
           PERFORM BUILD-CHARACTER-TABLE
           PERFORM WRITE-OUTPUT

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       READ-SEQUENCES.
           READ INPUT-FILE INTO WS-SEQ-1
               AT END MOVE "Y" TO WS-EOF
           READ INPUT-FILE INTO WS-SEQ-2
               AT END MOVE "Y" TO WS-EOF
           READ INPUT-FILE INTO WS-SEQ-3
               AT END MOVE "Y" TO WS-EOF
           READ INPUT-FILE INTO WS-SEQ-4
               AT END MOVE "Y" TO WS-EOF
           READ INPUT-FILE INTO WS-SEQ-5
               AT END MOVE "Y" TO WS-EOF
           READ INPUT-FILE INTO WS-SEQ-6
               AT END MOVE "Y" TO WS-EOF
           READ INPUT-FILE INTO WS-SEQ-7
               AT END MOVE "Y" TO WS-EOF
           READ INPUT-FILE INTO WS-SEQ-8
               AT END MOVE "Y" TO WS-EOF
           READ INPUT-FILE INTO WS-SEQ-9
               AT END MOVE "Y" TO WS-EOF
           READ INPUT-FILE INTO WS-SEQ-10
               AT END MOVE "Y" TO WS-EOF

           COMPUTE WS-SEQUENCE-COUNT = 10
           COMPUTE WS-LENGTH = FUNCTION LENGTH(WS-SEQ-1).

       BUILD-CHARACTER-TABLE.
           PERFORM VARYING WS-POS FROM 1 BY 1
               UNTIL WS-POS > WS-LENGTH
               MOVE SPACES TO WS-TABLE-ROW
               PERFORM BUILD-ROW
               MOVE WS-TABLE-ROW TO WS-TABLE-ROW-WS-TABLE-INDEX
               ADD 1 TO WS-TABLE-INDEX
           END-PERFORM.

       BUILD-ROW.
           MOVE "0000" TO WS-CHAR-FOUND
           PERFORM CHECK-CHARACTER WITH TEST AFTER
               VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-SEQUENCE-COUNT
               MOVE WS-SEQ-WS-INDEX(WS-POS:1) TO WS-CHAR
               PERFORM SET-CHAR-FLAG
           END-PERFORM.

       SET-CHAR-FLAG.
           IF WS-CHAR = "A" OR WS-CHAR = "a"
               MOVE "1" TO WS-CHAR-FOUND(1:1)
           ELSE IF WS-CHAR = "C" OR WS-CHAR = "c"
               MOVE "1" TO WS-CHAR-FOUND(2:1)
           ELSE IF WS-CHAR = "G" OR WS-CHAR = "g"
               MOVE "1" TO WS-CHAR-FOUND(3:1)
           ELSE IF WS-CHAR = "T" OR WS-CHAR = "t"
               MOVE "1" TO WS-CHAR-FOUND(4:1)
           END-IF.

       WRITE-OUTPUT.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-LENGTH
               MOVE WS-TABLE-ROW-WS-INDEX TO OUTPUT-RECORD
               WRITE OUTPUT-RECORD
           END-PERFORM.

       END PROGRAM CHARACTER-TABLE.
```

## Alternative Simpler Version

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHARACTER-TABLE-SIMPLE.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "sequences.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-SEQUENCES.
          05 WS-SEQ-1 PIC X(50).
          05 WS-SEQ-2 PIC X(50).
          05 WS-SEQ-3 PIC X(50).
          05 WS-SEQ-4 PIC X(50).
          05 WS-SEQ-5 PIC X(50).
          05 WS-SEQ-6 PIC X(50).

       01 WS-LENGTH PIC 99 VALUE 0.
       01 WS-POS PIC 99 VALUE 1.
       01 WS-INDEX PIC 99 VALUE 1.
       01 WS-CHAR PIC X(1).
       01 WS-RESULT PIC X(100).
       01 WS-EOF PIC X VALUE "N".
       01 WS-CHAR-FOUND PIC X(4) VALUE "0000".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE

           READ INPUT-FILE INTO WS-SEQ-1
           READ INPUT-FILE INTO WS-SEQ-2
           READ INPUT-FILE INTO WS-SEQ-3
           READ INPUT-FILE INTO WS-SEQ-4
           READ INPUT-FILE INTO WS-SEQ-5
           READ INPUT-FILE INTO WS-SEQ-6

           COMPUTE WS-LENGTH = FUNCTION LENGTH(WS-SEQ-1)

           PERFORM VARYING WS-POS FROM 1 BY 1
               UNTIL WS-POS > WS-LENGTH
               MOVE SPACES TO WS-RESULT
               PERFORM BUILD-CHARACTER-ROW
               DISPLAY WS-RESULT
           END-PERFORM

           CLOSE INPUT-FILE
           STOP RUN.

       BUILD-CHARACTER-ROW.
           MOVE "0000" TO WS-CHAR-FOUND
           PERFORM CHECK-ALL-SEQUENCES
           MOVE WS-CHAR-FOUND TO WS-RESULT.

       CHECK-ALL-SEQUENCES.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 6
               MOVE WS-SEQ-WS-INDEX(WS-POS:1) TO WS-CHAR
               IF WS-CHAR = "A" OR WS-CHAR = "a"
                   MOVE "1" TO WS-CHAR-FOUND(1:1)
               ELSE IF WS-CHAR = "C" OR WS-CHAR = "c"
                   MOVE "1" TO WS-CHAR-FOUND(2:1)
               ELSE IF WS-CHAR = "G" OR WS-CHAR = "g"
                   MOVE "1" TO WS-CHAR-FOUND(3:1)
               ELSE IF WS-CHAR = "T" OR WS-CHAR = "t"
                   MOVE "1" TO WS-CHAR-FOUND(4:1)
               END-IF
           END-PERFORM.
```

## Explanation

This COBOL solution:

1. **Reads DNA sequences** from an input file containing multiple sequences
2. **Determines the length** of the sequences (assuming all are same length)
3. **For each position** in the sequences:
   - Checks which nucleotides (A, C, G, T) are present
   - Creates a binary representation showing presence/absence
4. **Outputs** the character table with one row per position

## Sample Input Format

```
ATCG
ATCG
ATCG
ATCG
ATCG
ATCG
```

## Sample Output Format

```
1000
1000
1000
1000
```

The program uses a simple binary approach where each position in the output represents the presence (1) or absence (0) of nucleotides A, C, G, T respectively.

