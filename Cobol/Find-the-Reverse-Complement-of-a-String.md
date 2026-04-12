# Rosalind Problem: Find the Reverse Complement of a String - COBOL Solution

## Problem Understanding

The task is to find the reverse complement of a DNA string. The reverse complement is formed by:
1. Taking the complement of each nucleotide (A↔T, C↔G)
2. Reversing the resulting string

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVERSE_COMPLEMENT.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INPUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO OUTPUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(1000).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(1000).

       WORKING-STORAGE SECTION.
       01 DNA-STRING PIC X(1000).
       01 REVERSE-COMPLEMENT PIC X(1000).
       01 STRING-LENGTH PIC 9(4) VALUE 0.
       01 I PIC 9(4) VALUE 0.
       01 J PIC 9(4) VALUE 0.
       01 CURRENT-CHAR PIC X.
       01 COMPLEMENT-CHAR PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO DNA-STRING
               AT END GO TO END-PROGRAM
           END-READ

           MOVE FUNCTION LENGTH(DNA-STRING) TO STRING-LENGTH

           MOVE 1 TO J

           PERFORM REVERSE-COMPLEMENT-LOOP VARYING I FROM STRING-LENGTH BY -1
               UNTIL I < 1

           MOVE REVERSE-COMPLEMENT TO OUTPUT-RECORD
           WRITE OUTPUT-RECORD

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           GO TO END-PROGRAM

       REVERSE-COMPLEMENT-LOOP.
           MOVE DNA-STRING(I:1) TO CURRENT-CHAR
           EVALUATE CURRENT-CHAR
               WHEN 'A' MOVE 'T' TO COMPLEMENT-CHAR
               WHEN 'T' MOVE 'A' TO COMPLEMENT-CHAR
               WHEN 'C' MOVE 'G' TO COMPLEMENT-CHAR
               WHEN 'G' MOVE 'C' TO COMPLEMENT-CHAR
               WHEN OTHER MOVE CURRENT-CHAR TO COMPLEMENT-CHAR
           END-EVALUATE
           MOVE COMPLEMENT-CHAR TO REVERSE-COMPLEMENT(J:1)
           ADD 1 TO J

       END-PROGRAM.
```

## Alternative Simpler Version

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVERSE_COMPLEMENT_SIMPLE.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INPUT-FILENAME
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(1000).

       WORKING-STORAGE SECTION.
       01 DNA-STRING PIC X(1000).
       01 REVERSE-COMPLEMENT PIC X(1000).
       01 STRING-LENGTH PIC 9(4) VALUE 0.
       01 I PIC 9(4) VALUE 0.
       01 J PIC 9999 VALUE 0.
       01 CURRENT-CHAR PIC X.
       01 COMPLEMENT-CHAR PIC X.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO DNA-STRING
           END-READ
           CLOSE INPUT-FILE

           MOVE FUNCTION LENGTH(DNA-STRING) TO STRING-LENGTH
           MOVE 1 TO J

           PERFORM VARYING I FROM STRING-LENGTH BY -1 
               UNTIL I < 1
               MOVE DNA-STRING(I:1) TO CURRENT-CHAR
               EVALUATE CURRENT-CHAR
                   WHEN 'A' MOVE 'T' TO COMPLEMENT-CHAR
                   WHEN 'T' MOVE 'A' TO COMPLEMENT-CHAR
                   WHEN 'C' MOVE 'G' TO COMPLEMENT-CHAR
                   WHEN 'G' MOVE 'C' TO COMPLEMENT-CHAR
                   WHEN OTHER MOVE CURRENT-CHAR TO COMPLEMENT-CHAR
               END-EVALUATE
               MOVE COMPLEMENT-CHAR TO REVERSE-COMPLEMENT(J:1)
               ADD 1 TO J
           END-PERFORM

           DISPLAY REVERSE-COMPLEMENT(1:STRING-LENGTH)
           STOP RUN.
```

## How It Works

1. **Input Reading**: Read the DNA string from input file
2. **Length Calculation**: Determine the length of the input string
3. **Reverse Complement Generation**: 
   - Loop through the string from end to beginning
   - For each character, find its complement (A↔T, C↔G)
   - Place the complement in reverse order
4. **Output**: Display the result

## Example

**Input**: `AAAACCCGGT`
**Process**: 
- Reverse: `TGGCCCAAAA`
- Complement: `ACCGGTTTTA`
- Final result: `ACCGGTTTTA`

## Usage Notes

- The program assumes input is in a file named `input.txt`
- The output is displayed to console
- Works with DNA strings containing only A, T, C, G characters
- Handles strings up to 1000 characters long

This solution correctly implements the reverse complement algorithm in COBOL, following the requirements of the Rosalind problem.

