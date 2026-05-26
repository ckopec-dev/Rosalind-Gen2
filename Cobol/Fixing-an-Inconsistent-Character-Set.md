# Rosalind Problem: Fixing an Inconsistent Character Set (COBOL Solution)

## Problem Understanding

The problem requires us to identify and fix inconsistent characters in a character set. Given a set of strings with the same length, we need to:
1. Identify which characters are inconsistent (appear in different positions across strings)
2. Fix the inconsistencies by choosing the most frequent character at each position

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIX-INCONSISTENT-CHARACTER-SET.
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
       01 INPUT-RECORD PIC A(100).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC A(100).

       WORKING-STORAGE SECTION.
       01 WS-STRING-LENGTH PIC 99 VALUE 0.
       01 WS-NUM-STRINGS PIC 99 VALUE 0.
       01 WS-CHARACTER-COUNTS.
           05 WS-CHAR-COUNTS OCCURS 26 TIMES PIC 99 VALUE 0.
       01 WS-STRING-LIST.
           05 WS-STRINGS OCCURS 100 TIMES PIC A(100).
       01 WS-MAJORITY-CHARS PIC A(100) VALUE SPACES.
       01 WS-TEMP-CHAR PIC A(1).
       01 WS-CHAR-INDEX PIC 99 VALUE 0.
       01 WS-STRING-INDEX PIC 99 VALUE 0.
       01 WS-POS-INDEX PIC 99 VALUE 0.
       01 WS-MAX-COUNT PIC 99 VALUE 0.
       01 WS-CHAR-FOUND PIC X VALUE "N".
       01 WS-EOF PIC X VALUE "N".
       01 WS-TEMP-RECORD PIC A(100).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-PROGRAM.
           PERFORM READ-INPUT-FILE.
           PERFORM PROCESS-CHARACTER-SET.
           PERFORM WRITE-OUTPUT-FILE.
           STOP RUN.

       INITIALIZE-PROGRAM.
           MOVE "N" TO WS-EOF.
           MOVE 0 TO WS-NUM-STRINGS.
           MOVE 0 TO WS-STRING-LENGTH.

       READ-INPUT-FILE.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO WS-TEMP-RECORD
               AT END MOVE "Y" TO WS-EOF
           END-READ.

           PERFORM UNTIL WS-EOF = "Y"
               IF WS-TEMP-RECORD NOT = SPACES
                   ADD 1 TO WS-NUM-STRINGS
                   MOVE WS-TEMP-RECORD TO WS-STRINGS(WS-NUM-STRINGS)
                   IF WS-STRING-LENGTH = 0
                       MOVE FUNCTION LENGTH(WS-TEMP-RECORD) TO WS-STRING-LENGTH
                   END-IF
               END-IF
               READ INPUT-FILE INTO WS-TEMP-RECORD
                   AT END MOVE "Y" TO WS-EOF
               END-READ
           END-PERFORM.

           CLOSE INPUT-FILE.

       PROCESS-CHARACTER-SET.
           PERFORM VARYING WS-POS-INDEX FROM 1 BY 1 UNTIL WS-POS-INDEX > WS-STRING-LENGTH
               PERFORM CLEAR-COUNTS.
               PERFORM VARYING WS-STRING-INDEX FROM 1 BY 1 UNTIL WS-STRING-INDEX > WS-NUM-STRINGS
                   MOVE WS-STRINGS(WS-STRING-INDEX) TO WS-TEMP-RECORD.
                   SUBTRACT 1 FROM WS-POS-INDEX GIVING WS-CHAR-INDEX.
                   MOVE WS-TEMP-RECORD(WS-POS-INDEX:1) TO WS-TEMP-CHAR.
                   IF WS-TEMP-CHAR NOT = " "
                       SUBTRACT 65 FROM FUNCTION ORD(WS-TEMP-CHAR) GIVING WS-CHAR-INDEX
                       ADD 1 TO WS-CHAR-COUNTS(WS-CHAR-INDEX)
                   END-IF
               END-PERFORM.
               PERFORM FIND-MAJORITY-CHAR.
           END-PERFORM.

       CLEAR-COUNTS.
           PERFORM VARYING WS-CHAR-INDEX FROM 1 BY 1 UNTIL WS-CHAR-INDEX > 26
               MOVE 0 TO WS-CHAR-COUNTS(WS-CHAR-INDEX)
           END-PERFORM.

       FIND-MAJORITY-CHAR.
           MOVE 0 TO WS-MAX-COUNT.
           MOVE " " TO WS-MAJORITY-CHARS(WS-POS-INDEX:1).
           PERFORM VARYING WS-CHAR-INDEX FROM 1 BY 1 UNTIL WS-CHAR-INDEX > 26
               IF WS-CHAR-COUNTS(WS-CHAR-INDEX) > WS-MAX-COUNT
                   MOVE WS-CHAR-COUNTS(WS-CHAR-INDEX) TO WS-MAX-COUNT
                   ADD 65 TO WS-CHAR-INDEX GIVING WS-CHAR-INDEX
                   MOVE FUNCTION CHAR(WS-CHAR-INDEX) TO WS-MAJORITY-CHARS(WS-POS-INDEX:1)
               END-IF
           END-PERFORM.

       WRITE-OUTPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           MOVE WS-MAJORITY-CHARS TO OUTPUT-RECORD.
           WRITE OUTPUT-RECORD.
           CLOSE OUTPUT-FILE.
```

## Explanation

This COBOL program solves the inconsistent character set problem by:

1. **Reading Input**: Reads strings from an input file and stores them in an array
2. **Processing Each Position**: For each character position across all strings:
   - Counts occurrences of each character
   - Determines the most frequent character (majority)
3. **Output**: Writes the fixed character set to output file

## Key Features

- **Array Handling**: Uses OCCURS clauses to manage multiple strings and character counts
- **Character Processing**: Converts characters to ASCII values for counting
- **Majority Voting**: Finds the most frequent character at each position
- **File I/O**: Properly opens and closes input/output files

## Usage

1. Create input file named `input.txt` with strings (one per line)
2. Compile and run the COBOL program
3. Output will be written to `output.txt`

The program handles the core requirement of fixing inconsistent character sets by finding the most representative character at each position across the dataset.

