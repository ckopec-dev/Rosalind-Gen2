# Rosalind Problem: Reconstruct a String from its Burrows-Wheeler Transform (BWT)
## Solution in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECONSTRUCT-BWT.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "bwt_input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "bwt_output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC A(1000).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC A(1000).

       WORKING-STORAGE SECTION.
       01 BWT-STRING PIC A(1000).
       01 BWT-LENGTH PIC 9(4) VALUE 0.
       01 I PIC 9(4) VALUE 1.
       01 J PIC 9(4) VALUE 1.
       01 K PIC 9(4) VALUE 1.
       01 TEMP-CHAR PIC A(1).
       01 TEMP-STRING PIC A(1000).
       01 INDEX-ARRAY OCCURS 1000 TIMES INDEXED BY IDX.
           02 INDEX-VALUE PIC 9(4).
       01 SORT-ARRAY OCCURS 1000 TIMES INDEXED BY SORT-IDX.
           02 SORT-CHAR PIC A(1).
           02 SORT-INDEX PIC 9(4).
       01 SORT-FLAG PIC X VALUE "N".
       01 TEMP-INDEX PIC 9(4) VALUE 0.
       01 RESULT-STRING PIC A(1000).
       01 COUNT-ARRAY OCCURS 256 TIMES INDEXED BY CHAR-INDEX.
           02 CHAR-COUNT PIC 9(4) VALUE 0.
       01 CUMULATIVE-ARRAY OCCURS 256 TIMES INDEXED BY CUM-INDEX.
           02 CUMULATIVE-COUNT PIC 9(4) VALUE 0.
       01 FIRST-CHAR-INDEX PIC 9(4) VALUE 0.
       01 CURRENT-INDEX PIC 9(4) VALUE 0.
       01 EOF-FLAG PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO BWT-STRING
               AT END MOVE "Y" TO EOF-FLAG
           END-READ

           IF EOF-FLAG = "N"
               PERFORM PROCESS-BWT
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PROCESS-BWT.
           MOVE FUNCTION LENGTH(BWT-STRING) TO BWT-LENGTH
           MOVE 1 TO I

           * Count character frequencies
           PERFORM COUNT-FREQUENCIES

           * Build cumulative array
           PERFORM BUILD-CUMULATIVE

           * Find first character index
           PERFORM FIND-FIRST-CHARACTER

           * Reconstruct the original string
           PERFORM RECONSTRUCT-STRING

           * Write result to output file
           MOVE RESULT-STRING TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.

       COUNT-FREQUENCIES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BWT-LENGTH
               MOVE BWT-STRING(I:1) TO TEMP-CHAR
               COMPUTE CHAR-INDEX(TEMP-CHAR) = CHAR-INDEX(TEMP-CHAR) + 1
           END-PERFORM.

       BUILD-CUMULATIVE.
           MOVE 0 TO CUMULATIVE-COUNT(1)
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > 256
               COMPUTE CUMULATIVE-COUNT(I) = 
                   CUMULATIVE-COUNT(I-1) + CHAR-COUNT(I-1)
           END-PERFORM.

       FIND-FIRST-CHARACTER.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BWT-LENGTH
               IF BWT-STRING(I:1) = "A"
                   MOVE I TO FIRST-CHAR-INDEX
                   GO TO FIND-FIRST-CHARACTER-EXIT
               END-IF
           END-PERFORM.
       FIND-FIRST-CHARACTER-EXIT.

       RECONSTRUCT-STRING.
           MOVE 0 TO CURRENT-INDEX
           MOVE 1 TO I

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BWT-LENGTH
               MOVE BWT-STRING(FIRST-CHAR-INDEX:1) TO TEMP-CHAR
               MOVE TEMP-CHAR TO RESULT-STRING(I:1)
               COMPUTE CURRENT-INDEX = 
                   CUMULATIVE-COUNT(FUNCTION ORD(BWT-STRING(FIRST-CHAR-INDEX:1))) + 
                   CHAR-COUNT(FUNCTION ORD(BWT-STRING(FIRST-CHAR-INDEX:1))) - 1
               MOVE CURRENT-INDEX TO FIRST-CHAR-INDEX
           END-PERFORM.

       * Alternative reconstruction approach (more direct)
       RECONSTRUCT-STRING-ALT.
           MOVE BWT-STRING TO TEMP-STRING
           MOVE 0 TO CURRENT-INDEX
           MOVE 1 TO I

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > BWT-LENGTH
               COMPUTE CURRENT-INDEX = 
                   CUMULATIVE-COUNT(FUNCTION ORD(BWT-STRING(I:1))) + 
                   CHAR-COUNT(FUNCTION ORD(BWT-STRING(I:1))) - 1
               MOVE BWT-STRING(I:1) TO TEMP-CHAR
               MOVE TEMP-CHAR TO RESULT-STRING(CURRENT-INDEX:1)
           END-PERFORM.
```

This COBOL solution implements the Burrows-Wheeler Transform (BWT) reconstruction algorithm. The approach follows these key steps:

1. **Read Input**: Read the BWT string from input file
2. **Character Counting**: Count frequency of each character in the BWT
3. **Cumulative Array**: Build cumulative count array for character positions
4. **Reconstruction**: Use the BWT and cumulative arrays to reconstruct the original string
5. **Output**: Write the reconstructed string to output file

The algorithm works by:
- Using the cumulative array to determine where each character should be placed
- Following the BWT transformation rules in reverse
- Building the original string character by character

Note: This is a simplified implementation that assumes the input is properly formatted and handles basic BWT reconstruction. For production use, additional error checking and edge case handling would be necessary.

