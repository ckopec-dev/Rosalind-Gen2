# Rosalind Problem Quartets in COBOL

## Problem Understanding

The Quartets problem asks us to find all quartets (sets of 4 taxa) from a given set of taxa that are compatible with a given tree structure. In phylogenetics, quartets are used to represent relationships between four taxa.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUARTETS.
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
       01 INPUT-LINE PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE PIC X(100).

       WORKING-STORAGE SECTION.
       01 TAXA-LIST.
          05 TAXA OCCURS 20 TIMES PIC X(10).
       01 TAXA-COUNT PIC 99 VALUE 0.
       01 TAXA-INDEX1 PIC 99 VALUE 0.
       01 TAXA-INDEX2 PIC 99 VALUE 0.
       01 TAXA-INDEX3 PIC 99 VALUE 0.
       01 TAXA-INDEX4 PIC 99 VALUE 0.
       01 QUARTET-STRING PIC X(50).
       01 EOF-FLAG PIC X VALUE 'N'.
       01 LINE-LENGTH PIC 99 VALUE 0.
       01 I PIC 99 VALUE 0.
       01 J PIC 99 VALUE 0.
       01 K PIC 99 VALUE 0.
       01 L PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           
           PERFORM READ-TAXA
           
           PERFORM GENERATE-QUARTETS
           
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       READ-TAXA.
           READ INPUT-FILE INTO INPUT-LINE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ.
           
           IF EOF-FLAG = 'N'
               PERFORM PARSE-TAXA-LINE
           END-IF.

       PARSE-TAXA-LINE.
           MOVE 0 TO TAXA-COUNT
           MOVE 1 TO I
           
           PERFORM UNTIL I > 100 OR TAXA-COUNT >= 20
               IF INPUT-LINE(I:1) = ' ' OR INPUT-LINE(I:1) = '.'
                   ADD 1 TO TAXA-COUNT
                   MOVE SPACES TO TAXA(TAXA-COUNT)
                   MOVE 1 TO J
                   PERFORM UNTIL INPUT-LINE(I:1) = ' ' OR INPUT-LINE(I:1) = '.'
                       IF I > 100 THEN EXIT PERFORM
                       MOVE INPUT-LINE(I:1) TO TAXA(TAXA-COUNT)(J:1)
                       ADD 1 TO I
                       ADD 1 TO J
                   END-PERFORM
                   ADD 1 TO I
               ELSE
                   ADD 1 TO I
               END-IF
           END-PERFORM.

       GENERATE-QUARTETS.
           PERFORM VARYING TAXA-INDEX1 FROM 1 BY 1 UNTIL TAXA-INDEX1 > TAXA-COUNT
               PERFORM VARYING TAXA-INDEX2 FROM (TAXA-INDEX1 + 1) BY 1 UNTIL TAXA-INDEX2 > TAXA-COUNT
                   PERFORM VARYING TAXA-INDEX3 FROM (TAXA-INDEX2 + 1) BY 1 UNTIL TAXA-INDEX3 > TAXA-COUNT
                       PERFORM VARYING TAXA-INDEX4 FROM (TAXA-INDEX3 + 1) BY 1 UNTIL TAXA-INDEX4 > TAXA-COUNT
                           PERFORM FORM-QUARTET-STRING
                           PERFORM WRITE-QUARTET
                       END-PERFORM
                   END-PERFORM
               END-PERFORM
           END-PERFORM.

       FORM-QUARTET-STRING.
           MOVE SPACES TO QUARTET-STRING
           MOVE TAXA(TAXA-INDEX1) TO QUARTET-STRING(1:10)
           MOVE ' ' TO QUARTET-STRING(11:1)
           MOVE TAXA(TAXA-INDEX2) TO QUARTET-STRING(12:10)
           MOVE ' ' TO QUARTET-STRING(22:1)
           MOVE TAXA(TAXA-INDEX3) TO QUARTET-STRING(23:10)
           MOVE ' ' TO QUARTET-STRING(33:1)
           MOVE TAXA(TAXA-INDEX4) TO QUARTET-STRING(34:10).

       WRITE-QUARTET.
           WRITE OUTPUT-LINE FROM QUARTET-STRING.
           IF OUTPUT-LINE = SPACES
               DISPLAY "Error writing quartet"
           END-IF.

       END PROGRAM QUARTETS.
```

## Explanation

This COBOL program solves the Quartets problem by:

1. **Reading Input**: The program reads taxa from an input file, parsing them into an array
2. **Generating Combinations**: Using nested loops, it generates all possible combinations of 4 taxa from the available taxa
3. **Formatting Output**: Each quartet is formatted as a string with the 4 taxa separated by spaces
4. **Writing Results**: All quartets are written to an output file

## Key Features

- **Data Structure**: Uses an array `TAXA` to store taxa names
- **Combination Logic**: Implements nested loops to generate all 4-element combinations
- **String Handling**: Properly formats quartet strings with spaces between taxa
- **File I/O**: Handles both input and output file operations

## Usage

1. Create an input file named `input.txt` with taxa names separated by spaces
2. Compile and run the COBOL program
3. Output will be written to `output.txt` containing all possible quartets

## Time Complexity

O(n⁴) where n is the number of taxa, since we generate all combinations of 4 taxa from n taxa.

## Space Complexity

O(n) for storing the taxa list and O(1) for the quartet generation process.

