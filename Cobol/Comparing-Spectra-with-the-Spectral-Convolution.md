# Rosalind Problem: Comparing Spectra with the Spectral Convolution

## Problem Description
The spectral convolution of two spectra is the multiset of differences between their masses. Given two spectra, we need to compute their spectral convolution.

## Solution in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPARING-SPECTRA.
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
       01 I PIC 99 VALUE 1.
       01 J PIC 99 VALUE 1.
       01 K PIC 99 VALUE 1.
       01 N PIC 99 VALUE 0.
       01 M PIC 99 VALUE 0.
       01 TEMP-DIFF PIC 9999V99.
       01 TEMP-INT PIC 9999.
       01 TEMP-STR PIC X(10).
       01 EOF-FLAG PIC X VALUE 'N'.
       01 SPECTRUM1 OCCURS 50 TIMES PIC 9999V99.
       01 SPECTRUM2 OCCURS 50 TIMES PIC 9999V99.
       01 CONVOLUTION OCCURS 2500 TIMES PIC 9999V99.
       01 CONVOLUTION-COUNT PIC 9999 VALUE 0.
       01 LINE-START PIC 99 VALUE 1.
       01 LINE-END PIC 99 VALUE 1.
       01 CHAR-POS PIC 99 VALUE 1.
       01 NUM-STR PIC X(10).
       01 NUM-VALUE PIC 9999V99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM READ-INPUT-LINES

           PERFORM COMPUTE-CONVOLUTION

           PERFORM WRITE-OUTPUT

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       READ-INPUT-LINES.
           READ INPUT-FILE INTO INPUT-LINE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           IF EOF-FLAG = 'N'
               PERFORM PARSE-SPECTRUM1
           END-IF.

           READ INPUT-FILE INTO INPUT-LINE
               AT END MOVE 'Y' TO EOF-FLAG
           END-READ.

           IF EOF-FLAG = 'N'
               PERFORM PARSE-SPECTRUM2
           END-IF.

       PARSE-SPECTRUM1.
           MOVE 0 TO N
           MOVE 1 TO LINE-START
           MOVE 1 TO CHAR-POS

           PERFORM UNTIL LINE-START > LENGTH OF INPUT-LINE
               MOVE LINE-START TO LINE-END
               PERFORM FIND-END-OF-NUMBER
               MOVE 0 TO NUM-VALUE
               MOVE INPUT-LINE(SUBSCRIPT:LINE-END-LINE-START+1) TO NUM-STR
               COMPUTE NUM-VALUE = FUNCTION NUMVAL(NUM-STR)
               ADD 1 TO N
               MOVE NUM-VALUE TO SPECTRUM1(N)
               MOVE LINE-END TO LINE-START
               ADD 1 TO LINE-START
               MOVE 1 TO CHAR-POS
           END-PERFORM.

       PARSE-SPECTRUM2.
           MOVE 0 TO M
           MOVE 1 TO LINE-START
           MOVE 1 TO CHAR-POS

           PERFORM UNTIL LINE-START > LENGTH OF INPUT-LINE
               MOVE LINE-START TO LINE-END
               PERFORM FIND-END-OF-NUMBER
               MOVE 0 TO NUM-VALUE
               MOVE INPUT-LINE(SUBSCRIPT:LINE-END-LINE-START+1) TO NUM-STR
               COMPUTE NUM-VALUE = FUNCTION NUMVAL(NUM-STR)
               ADD 1 TO M
               MOVE NUM-VALUE TO SPECTRUM2(M)
               MOVE LINE-END TO LINE-START
               ADD 1 TO LINE-START
               MOVE 1 TO CHAR-POS
           END-PERFORM.

       FIND-END-OF-NUMBER.
           PERFORM UNTIL CHAR-POS > LENGTH OF INPUT-LINE OR
                   INPUT-LINE(CHAR-POS:1) = ' '
               ADD 1 TO CHAR-POS
           END-PERFORM
           MOVE CHAR-POS TO LINE-END.

       COMPUTE-CONVOLUTION.
           MOVE 0 TO CONVOLUTION-COUNT

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > M
                   COMPUTE TEMP-DIFF = SPECTRUM1(I) - SPECTRUM2(J)
                   ADD 1 TO CONVOLUTION-COUNT
                   MOVE TEMP-DIFF TO CONVOLUTION(CONVOLUTION-COUNT)
               END-PERFORM
           END-PERFORM.

       WRITE-OUTPUT.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > CONVOLUTION-COUNT
               MOVE CONVOLUTION(K) TO TEMP-INT
               MOVE TEMP-INT TO TEMP-STR
               MOVE TEMP-STR TO OUTPUT-LINE
               WRITE OUTPUT-LINE
           END-PERFORM.

       END PROGRAM COMPARING-SPECTRA.
```

## Explanation

This COBOL program solves the spectral convolution problem by:

1. **Reading Input**: The program reads two spectra from input files, parsing them into arrays of mass values.

2. **Computing Convolution**: For each mass in spectrum1 and each mass in spectrum2, it computes their difference and stores it in the convolution array.

3. **Output**: The program writes all computed differences to the output file.

## Key Features

- **Data Structures**: Uses arrays to store the two spectra and the convolution results
- **Input Parsing**: Handles parsing of space-separated numbers from input lines
- **Convolution Computation**: Computes all pairwise differences between the two spectra
- **Output Generation**: Writes results in the required format

## Usage

1. Create an input file named "input.txt" with two lines:
   - First line: space-separated masses for spectrum1
   - Second line: space-separated masses for spectrum2
2. Compile and run the COBOL program
3. Output will be written to "output.txt" with all convolution results

## Time Complexity
O(n × m) where n and m are the sizes of the two spectra.

## Space Complexity
O(n × m) for storing the convolution results.

