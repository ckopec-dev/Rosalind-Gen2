# Rosalind Problem: Identifying Reversing Substitutions (COBOL Solution)

## Problem Understanding

The problem asks us to identify reversing substitutions in DNA sequences. A reversing substitution occurs when a nucleotide changes to its complement (A↔T, C↔G) but the resulting sequence is a palindrome.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDENTIFY-REVERSING-SUBSTITUTIONS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_revc.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 DNA-LENGTH PIC 999 VALUE 0.
       01 I PIC 999 VALUE 1.
       01 J PIC 999 VALUE 1.
       01 K PIC 999 VALUE 1.
       01 TEMP-CHAR PIC X VALUE SPACE.
       01 REVERSE-STR PIC X(100) VALUE SPACES.
       01 COMPLEMENT-STR PIC X(100) VALUE SPACES.
       01 ORIGINAL-STR PIC X(100) VALUE SPACES.
       01 IS-PALINDROME PIC X VALUE "N".
       01 FOUND-SUBSTITUTIONS PIC 999 VALUE 0.
       01 RESULT-STR PIC X(100) VALUE SPACES.

       01 DNA-CHARS.
           05 DNA-CHAR PIC X OCCURS 100 TIMES.

       01 COMPLEMENT-CHARS.
           05 COMPLEMENT-CHAR PIC X OCCURS 100 TIMES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO ORIGINAL-STR
               AT END GO TO END-PROGRAM
           END-READ

           MOVE FUNCTION LENGTH(ORIGINAL-STR) TO DNA-LENGTH

           * Convert to uppercase and remove spaces
           PERFORM CONVERT-TO-UPPERCASE
           PERFORM REMOVE-SPACES

           MOVE FUNCTION LENGTH(ORIGINAL-STR) TO DNA-LENGTH

           * Generate complement string
           PERFORM GENERATE-COMPLEMENT

           * Check for palindromes at each position
           PERFORM CHECK-PALINDROMES

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE

           GO TO END-PROGRAM.

       CONVERT-TO-UPPERCASE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DNA-LENGTH
               IF ORIGINAL-STR(I:1) = "a" OR ORIGINAL-STR(I:1) = "A"
                   MOVE "A" TO ORIGINAL-STR(I:1)
               ELSE IF ORIGINAL-STR(I:1) = "t" OR ORIGINAL-STR(I:1) = "T"
                   MOVE "T" TO ORIGINAL-STR(I:1)
               ELSE IF ORIGINAL-STR(I:1) = "c" OR ORIGINAL-STR(I:1) = "C"
                   MOVE "C" TO ORIGINAL-STR(I:1)
               ELSE IF ORIGINAL-STR(I:1) = "g" OR ORIGINAL-STR(I:1) = "G"
                   MOVE "G" TO ORIGINAL-STR(I:1)
               END-IF
           END-PERFORM.

       REMOVE-SPACES.
           MOVE 1 TO I
           MOVE 1 TO J
           PERFORM UNTIL I > DNA-LENGTH
               IF ORIGINAL-STR(I:1) NOT = " "
                   MOVE ORIGINAL-STR(I:1) TO ORIGINAL-STR(J:1)
                   ADD 1 TO J
               END-IF
               ADD 1 TO I
           END-PERFORM
           SUBTRACT 1 FROM J
           MOVE J TO DNA-LENGTH.

       GENERATE-COMPLEMENT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DNA-LENGTH
               IF ORIGINAL-STR(I:1) = "A"
                   MOVE "T" TO COMPLEMENT-STR(I:1)
               ELSE IF ORIGINAL-STR(I:1) = "T"
                   MOVE "A" TO COMPLEMENT-STR(I:1)
               ELSE IF ORIGINAL-STR(I:1) = "C"
                   MOVE "G" TO COMPLEMENT-STR(I:1)
               ELSE IF ORIGINAL-STR(I:1) = "G"
                   MOVE "C" TO COMPLEMENT-STR(I:1)
               END-IF
           END-PERFORM.

       CHECK-PALINDROMES.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DNA-LENGTH - 3
               PERFORM VARYING J FROM I BY 1 UNTIL J > I + 3
                   IF ORIGINAL-STR(J:1) = COMPLEMENT-STR(DNA-LENGTH - J + 1:1)
                       MOVE "Y" TO IS-PALINDROME
                   ELSE
                       MOVE "N" TO IS-PALINDROME
                       GO TO NEXT-POSITION
                   END-IF
               END-PERFORM

               * If we get here, it's a palindrome
               ADD 1 TO FOUND-SUBSTITUTIONS
               STRING "Position " I " - " INTO RESULT-STR
               STRING "Substitution found" INTO RESULT-STR
               WRITE OUTPUT-RECORD FROM RESULT-STR
               NEXT-POSITION:
           END-PERFORM.

       END-PROGRAM.
           STOP RUN.
```

## Alternative Simpler Approach

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVERSING-SUBSTITUTIONS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_revc.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 DNA-STR PIC X(100) VALUE SPACES.
       01 DNA-LENGTH PIC 999 VALUE 0.
       01 I PIC 999 VALUE 1.
       01 J PIC 999 VALUE 1.
       01 K PIC 999 VALUE 1.
       01 COMPLEMENT-STR PIC X(100) VALUE SPACES.
       01 TEMP-CHAR PIC X VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE

           READ INPUT-FILE INTO DNA-STR
               AT END GO TO END-PROGRAM
           END-READ

           MOVE FUNCTION LENGTH(DNA-STR) TO DNA-LENGTH

           * Generate complement
           PERFORM GENERATE-COMPLEMENT

           * Find reversing substitutions
           PERFORM FIND-REVERSING-SUBSTITUTIONS

           CLOSE INPUT-FILE
           GO TO END-PROGRAM.

       GENERATE-COMPLEMENT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > DNA-LENGTH
               IF DNA-STR(I:1) = "A"
                   MOVE "T" TO COMPLEMENT-STR(I:1)
               ELSE IF DNA-STR(I:1) = "T"
                   MOVE "A" TO COMPLEMENT-STR(I:1)
               ELSE IF DNA-STR(I:1) = "C"
                   MOVE "G" TO COMPLEMENT-STR(I:1)
               ELSE IF DNA-STR(I:1) = "G"
                   MOVE "C" TO COMPLEMENT-STR(I:1)
               END-IF
           END-PERFORM.

       FIND-REVERSING-SUBSTITUTIONS.
           DISPLAY "Original: " DNA-STR
           DISPLAY "Complement: " COMPLEMENT-STR
           DISPLAY "Length: " DNA-LENGTH
           DISPLAY "Checking for reversing substitutions..."

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > (DNA-LENGTH - 3)
               IF (DNA-STR(I:1) = "A" AND COMPLEMENT-STR(DNA-LENGTH - I + 1:1) = "T") OR
                  (DNA-STR(I:1) = "T" AND COMPLEMENT-STR(DNA-LENGTH - I + 1:1) = "A") OR
                  (DNA-STR(I:1) = "C" AND COMPLEMENT-STR(DNA-LENGTH - I + 1:1) = "G") OR
                  (DNA-STR(I:1) = "G" AND COMPLEMENT-STR(DNA-LENGTH - I + 1:1) = "C")
                   DISPLAY "Reversing substitution at position " I
               END-IF
           END-PERFORM.

       END-PROGRAM.
           STOP RUN.
```

## Key Features of the Solution

1. **Input/Output Handling**: Reads DNA sequence from file and processes it
2. **Complement Generation**: Converts each nucleotide to its complement (A↔T, C↔G)
3. **Palindrome Checking**: Identifies positions where substitutions create palindromic patterns
4. **Position Tracking**: Records the positions of identified reversing substitutions

## Algorithm Explanation

1. Read the input DNA sequence
2. Generate the complement sequence by replacing each nucleotide with its complement
3. Check for reversing substitutions by comparing each position with its complement position
4. Output the positions where reversing substitutions occur

This COBOL solution handles the core requirements of identifying reversing substitutions in DNA sequences while maintaining proper file I/O and data processing structures.

