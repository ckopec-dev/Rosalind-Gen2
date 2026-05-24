# Rosalind Problem: Locating Restriction Sites (COBOL Solution)

## Problem Description
Given a DNA string, find all positions where a restriction site occurs. A restriction site is a palindromic sequence that reads the same forward and backward.

## Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOCATING-RESTRICTION-SITES.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_restr.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           05 DNA-STRING    PIC X(1000).
           05 DNA-LENGTH    PIC 9(4).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD    PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-DNA-ARRAY.
           05 WS-DNA-CHAR   OCCURS 1000 TIMES PIC X.
       01 WS-REVERSE-ARRAY.
           05 WS-REVERSE-CHAR OCCURS 1000 TIMES PIC X.
       01 WS-LENGTH        PIC 9(4).
       01 WS-START         PIC 9(4).
       01 WS-END           PIC 9(4).
       01 WS-TEMP          PIC X.
       01 WS-I             PIC 9(4).
       01 WS-J             PIC 9(4).
       01 WS-K             PIC 9(4).
       01 WS-FOUND         PIC X VALUE "N".
       01 WS-OUTPUT-TEXT   PIC X(50).
       01 WS-EOF           PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE
               AT END MOVE "Y" TO WS-EOF
           END-READ

           IF WS-EOF = "N"
               MOVE INPUT-RECORD TO WS-DNA-ARRAY
               PERFORM GET-DNA-LENGTH
               PERFORM FIND-RESTRICTION-SITES
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       GET-DNA-LENGTH.
           MOVE 0 TO WS-LENGTH
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > 1000 OR WS-DNA-CHAR(WS-I) = SPACE
               ADD 1 TO WS-LENGTH
           END-PERFORM.

       FIND-RESTRICTION-SITES.
           PERFORM VARYING WS-START FROM 1 BY 1
               UNTIL WS-START > WS-LENGTH - 3
               PERFORM VARYING WS-END FROM WS-START + 3 BY 1
                   UNTIL WS-END > WS-LENGTH OR WS-END > WS-START + 12
                   IF WS-END - WS-START + 1 >= 4
                       PERFORM CHECK-PALINDROME
                       IF WS-FOUND = "Y"
                           PERFORM WRITE-RESULT
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       CHECK-PALINDROME.
           MOVE "N" TO WS-FOUND
           MOVE 0 TO WS-J
           PERFORM VARYING WS-I FROM WS-START BY 1
               UNTIL WS-I > WS-END
               ADD 1 TO WS-J
               MOVE WS-DNA-CHAR(WS-I) TO WS-REVERSE-CHAR(WS-J)
           END-PERFORM

           PERFORM VARYING WS-I FROM WS-START BY 1
               UNTIL WS-I > WS-END
               COMPUTE WS-K = WS-END - WS-I + WS-START
               IF WS-DNA-CHAR(WS-I) NOT = WS-REVERSE-CHAR(WS-K)
                   GO TO PALINDROME-FAILED
               END-IF
           END-PERFORM
           MOVE "Y" TO WS-FOUND.

       PALINDROME-FAILED.
           CONTINUE.

       WRITE-RESULT.
           MOVE WS-START TO WS-OUTPUT-TEXT(1:4)
           MOVE " " TO WS-OUTPUT-TEXT(5:1)
           MOVE WS-END TO WS-OUTPUT-TEXT(6:4)
           MOVE " " TO WS-OUTPUT-TEXT(10:1)
           MOVE WS-END - WS-START + 1 TO WS-OUTPUT-TEXT(11:2)
           WRITE OUTPUT-RECORD FROM WS-OUTPUT-TEXT
               AFTER ADVANCING 1 LINE.
```

## Explanation

This COBOL program solves the restriction site locating problem by:

1. **Reading Input**: Reads DNA sequence from input file
2. **Finding Palindromes**: Checks all possible substrings of length 4-12 for palindromic properties
3. **Palindrome Verification**: Compares each substring with its reverse to verify palindrome property
4. **Output Results**: Writes positions and lengths of restriction sites to output file

## Key Features

- **Input/Output Handling**: Proper file handling for reading DNA sequence and writing results
- **Palindrome Detection**: Comprehensive palindrome checking algorithm
- **Range Validation**: Ensures restriction sites are between 4-12 base pairs
- **Position Tracking**: Records exact positions of restriction sites

## Usage

1. Save DNA sequence in file named "rosalind_restr.txt"
2. Compile and run the COBOL program
3. Results will be written to "output.txt"

The program handles the specific requirements of finding palindromic restriction sites in DNA sequences, which are crucial for molecular biology applications.

