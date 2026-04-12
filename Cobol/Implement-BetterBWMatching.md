# Rosalind Problem: Implement_BetterBWMatching in COBOL

## Problem Understanding

The BetterBWMatching problem involves implementing an efficient algorithm to count how many times a pattern occurs in a string using Burrows-Wheeler Transform (BWT) data structures. This requires implementing the BetterBWMatching algorithm that uses the LastToFirst mapping and counting arrays.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BETTERBWMATCHING.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD IN-FILE.
       01 IN-RECORD.
          05 TEXT-STRING     PIC A(1000).
          05 PATTERN-STRING  PIC A(100).
          05 PATTERN-COUNT   PIC 9(4).

       FD OUT-FILE.
       01 OUT-RECORD          PIC A(100).

       WORKING-STORAGE SECTION.
       01 TEXT-LENGTH         PIC 9(4) VALUE 0.
       01 PATTERN-LENGTH      PIC 9(4) VALUE 0.
       01 I                   PIC 9(4) VALUE 0.
       01 J                   PIC 9(4) VALUE 0.
       01 K                   PIC 9(4) VALUE 0.
       01 L                   PIC 9(4) VALUE 0.
       01 COUNT               PIC 9(4) VALUE 0.
       01 FIRST-POS           PIC 9(4) VALUE 0.
       01 LAST-POS            PIC 9(4) VALUE 0.
       01 SYMBOL              PIC A(1).
       01 TEMP-CHAR           PIC A(1).
       01 FOUND               PIC 9 VALUE 0.

       01 LAST-TO-FIRST-TABLE.
          05 LAST-TO-FIRST OCCURS 1000 TIMES INDEXED BY I-J.
             10 LAST-TO-FIRST-VALUE PIC 9(4).

       01 COUNT-ARRAY.
          05 COUNT-ARRAY-VALUE OCCURS 26 TIMES INDEXED BY I-K.
             10 COUNT-VALUE PIC 9(4).

       01 FIRST-ARRAY.
          05 FIRST-ARRAY-VALUE OCCURS 26 TIMES INDEXED BY I-L.
             10 FIRST-VALUE PIC 9(4).

       01 BWT-TEXT.
          05 BWT-CHARS OCCURS 1000 TIMES INDEXED BY I.
             10 BWT-CHAR PIC A(1).

       01 SORT-TEXT.
          05 SORT-CHARS OCCURS 1000 TIMES INDEXED BY I.
             10 SORT-CHAR PIC A(1).

       01 BWT-INDEX.
          05 BWT-INDEX-VALUE OCCURS 1000 TIMES INDEXED BY I.
             10 INDEX-VALUE PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT IN-FILE
           OPEN OUTPUT OUT-FILE

           READ IN-FILE INTO IN-RECORD
           IF EOF THEN GO TO END-PROGRAM
           
           MOVE TEXT-STRING TO BWT-TEXT
           MOVE PATTERN-STRING TO PATTERN-STRING
           MOVE PATTERN-COUNT TO PATTERN-LENGTH

           PERFORM BUILD-BWT-TEXT
           PERFORM BUILD-LAST-TO-FIRST
           PERFORM BUILD-COUNT-ARRAY
           PERFORM BUILD-FIRST-ARRAY

           PERFORM BETTER-BWMATCHING

           CLOSE IN-FILE
           CLOSE OUT-FILE
           STOP RUN.

       BUILD-BWT-TEXT.
           MOVE 0 TO TEXT-LENGTH
           PERFORM VARYING I BY 1
               UNTIL I > 1000 OR TEXT-STRING(I:1) = SPACES
               ADD 1 TO TEXT-LENGTH
           END-PERFORM

           PERFORM VARYING I BY 1
               UNTIL I > TEXT-LENGTH
               MOVE TEXT-STRING(I:1) TO BWT-CHARS(I)
           END-PERFORM.

       BUILD-LAST-TO-FIRST.
           PERFORM VARYING I BY 1
               UNTIL I > TEXT-LENGTH
               MOVE BWT-CHARS(I) TO TEMP-CHAR
               COMPUTE J = FUNCTION ORD(TEMP-CHAR) - FUNCTION ORD('A') + 1
               MOVE I TO LAST-TO-FIRST(I-J)
           END-PERFORM.

       BUILD-COUNT-ARRAY.
           PERFORM VARYING I-K BY 1
               UNTIL I-K > 26
               MOVE 0 TO COUNT-ARRAY-VALUE(I-K)
           END-PERFORM

           PERFORM VARYING I BY 1
               UNTIL I > TEXT-LENGTH
               COMPUTE J = FUNCTION ORD(BWT-CHARS(I)) - FUNCTION ORD('A') + 1
               ADD 1 TO COUNT-ARRAY-VALUE(J)
           END-PERFORM.

       BUILD-FIRST-ARRAY.
           MOVE 0 TO I-L
           PERFORM VARYING I-K BY 1
               UNTIL I-K > 26
               MOVE 0 TO COUNT-VALUE(I-K)
           END-PERFORM

           PERFORM VARYING I BY 1
               UNTIL I > TEXT-LENGTH
               COMPUTE J = FUNCTION ORD(BWT-CHARS(I)) - FUNCTION ORD('A') + 1
               ADD 1 TO COUNT-VALUE(J)
           END-PERFORM

           PERFORM VARYING I-K BY 1
               UNTIL I-K > 26
               ADD COUNT-VALUE(I-K) TO FIRST-VALUE(I-K)
           END-PERFORM.

       BETTER-BWMATCHING.
           MOVE PATTERN-LENGTH TO L
           MOVE 0 TO FIRST-POS
           MOVE TEXT-LENGTH TO LAST-POS
           MOVE 0 TO COUNT

           PERFORM VARYING I BY 1
               UNTIL I > L OR FIRST-POS > LAST-POS
               COMPUTE J = FUNCTION ORD(PATTERN-STRING(L:1)) - FUNCTION ORD('A') + 1
               MOVE PATTERN-STRING(L:1) TO SYMBOL
               SUBTRACT FIRST-VALUE(J) FROM LAST-POS
               ADD COUNT-VALUE(J) TO FIRST-POS
               SUBTRACT 1 FROM L
           END-PERFORM

           COMPUTE COUNT = LAST-POS - FIRST-POS + 1
           MOVE COUNT TO OUT-RECORD
           WRITE OUT-RECORD.

       END-PROGRAM.
```

## Explanation

This COBOL implementation solves the BetterBWMatching problem by:

1. **Reading Input**: Reads the BWT text and pattern from input file
2. **Building BWT Structure**: Creates the necessary arrays for the BWT algorithm
3. **Computing LastToFirst Mapping**: Builds the mapping from last column to first column positions
4. **Building Count Arrays**: Creates arrays for character counting in BWT
5. **Building First Arrays**: Computes the first positions of each character
6. **Performing BetterBWMatching**: Implements the core algorithm to count pattern occurrences

## Key Components

- **LastToFirst Mapping**: Maps positions in the last column to positions in the first column
- **Count Arrays**: Track how many times each character appears up to a given position
- **First Arrays**: Track the first occurrence position of each character
- **Pattern Matching**: Uses these structures to efficiently count pattern occurrences

## Time Complexity
O(n) where n is the length of the BWT text, making it much more efficient than naive pattern matching.

## Space Complexity
O(n) for storing the BWT structure and auxiliary arrays.

This solution efficiently implements the BetterBWMatching algorithm using COBOL's structured programming capabilities while maintaining the mathematical correctness required for bioinformatics applications.

