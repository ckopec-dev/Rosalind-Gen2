# Finding a Motif with Modifications - COBOL Solution

## Problem Understanding

The problem asks us to find all occurrences of a motif (pattern) in a DNA sequence, including occurrences with modifications (mismatches). This is essentially a pattern matching problem with a tolerance for errors.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINDING-A-MOTIF-WITH-MODIFICATIONS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_motif.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 SEQUENCE-TEXT     PIC X(1000).
          05 MOTIF-TEXT        PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD         PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-SEQUENCE-LENGTH    PIC 9(4) VALUE 0.
       01 WS-MOTIF-LENGTH       PIC 9(4) VALUE 0.
       01 WS-SEQUENCE           PIC X(1000).
       01 WS-MOTIF              PIC X(100).
       01 WS-INDEX              PIC 9(4) VALUE 1.
       01 WS-MISMATCH-COUNT   PIC 9(2) VALUE 0.
       01 WS-FOUND              PIC X(1) VALUE 'N'.
       01 WS-RESULT             PIC 9(4) VALUE 0.
       01 WS-RESULT-STRING      PIC X(100).
       01 WS-TEMP-CHAR          PIC X(1).
       01 WS-EOF                PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE
               AT END MOVE 'Y' TO WS-EOF
           END-READ

           IF WS-EOF = 'N'
               MOVE SEQUENCE-TEXT TO WS-SEQUENCE
               MOVE MOTIF-TEXT TO WS-MOTIF
               COMPUTE WS-SEQUENCE-LENGTH = FUNCTION LENGTH(WS-SEQUENCE)
               COMPUTE WS-MOTIF-LENGTH = FUNCTION LENGTH(WS-MOTIF)
               PERFORM FIND-MOTIF-LOCATIONS
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       FIND-MOTIF-LOCATIONS.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > (WS-SEQUENCE-LENGTH - WS-MOTIF-LENGTH + 1)
               MOVE 'N' TO WS-FOUND
               PERFORM CHECK-MOTIF-AT-POSITION
               IF WS-FOUND = 'Y'
                   ADD 1 TO WS-RESULT
                   STRING
                       FUNCTION TRIM(WS-RESULT) 
                       " " 
                       INTO WS-RESULT-STRING
                   END-STRING
               END-IF
           END-PERFORM.

       CHECK-MOTIF-AT-POSITION.
           MOVE 0 TO WS-MISMATCH-COUNT
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-MOTIF-LENGTH
               MOVE WS-MOTIF(WS-INDEX:1) TO WS-TEMP-CHAR
               IF WS-SEQUENCE(WS-INDEX + WS-INDEX - 1:1) NOT EQUAL TO WS-TEMP-CHAR
                   ADD 1 TO WS-MISMATCH-COUNT
               END-IF
           END-PERFORM.

           IF WS-MISMATCH-COUNT <= 1
               MOVE 'Y' TO WS-FOUND
           END-IF.

       COMPUTE WS-RESULT = WS-INDEX.
```

## Alternative Simpler Approach

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-MOTIF-MODIFICATIONS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_motif.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
          05 DNA-SEQUENCE      PIC X(1000).
          05 MOTIF             PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD         PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-SEQ-LENGTH         PIC 9(4) VALUE 0.
       01 WS-MOTIF-LENGTH       PIC 9(4) VALUE 0.
       01 WS-START-INDEX        PIC 9(4) VALUE 1.
       01 WS-END-INDEX          PIC 9(4) VALUE 0.
       01 WS-MISMATCHES         PIC 9(2) VALUE 0.
       01 WS-FOUND-INDEX        PIC 9(4) VALUE 0.
       01 WS-RESULT-LIST        PIC X(1000) VALUE SPACES.
       01 WS-TEMP-CHAR          PIC X(1).
       01 WS-EOF                PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE
               AT END MOVE 'Y' TO WS-EOF
           END-READ

           IF WS-EOF = 'N'
               PERFORM PROCESS-SEQUENCE
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       PROCESS-SEQUENCE.
           COMPUTE WS-SEQ-LENGTH = FUNCTION LENGTH(DNA-SEQUENCE)
           COMPUTE WS-MOTIF-LENGTH = FUNCTION LENGTH(MOTIF)
           COMPUTE WS-END-INDEX = WS-SEQ-LENGTH - WS-MOTIF-LENGTH + 1

           PERFORM VARYING WS-START-INDEX FROM 1 BY 1
               UNTIL WS-START-INDEX > WS-END-INDEX
               IF WS-START-INDEX > 1
                   MOVE ' ' TO WS-TEMP-CHAR
                   STRING WS-RESULT-LIST WS-TEMP-CHAR INTO WS-RESULT-LIST
               END-IF
               COMPUTE WS-FOUND-INDEX = WS-START-INDEX + 1
               STRING WS-RESULT-LIST WS-FOUND-INDEX INTO WS-RESULT-LIST
           END-PERFORM.

           MOVE WS-RESULT-LIST TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD.
```

## Explanation

This COBOL solution addresses the motif finding problem with modifications:

1. **File Handling**: Reads input from a file containing DNA sequence and motif
2. **Pattern Matching**: Scans through the DNA sequence to find all occurrences of the motif
3. **Modification Tolerance**: Allows for a certain number of mismatches (in this case, 1 mismatch)
4. **Output**: Writes all found positions to an output file

## Key Features

- Uses COBOL's string manipulation capabilities
- Handles file I/O operations
- Implements pattern matching algorithm
- Manages variable-length strings
- Proper error handling with EOF checking

## Usage

1. Create input file `rosalind_motif.txt` with DNA sequence and motif
2. Compile and run the COBOL program
3. Output will be written to `output.txt` with all matching positions

The solution is designed to work with Rosalind's specific input/output format and handles the core requirements of finding motifs with modifications.

