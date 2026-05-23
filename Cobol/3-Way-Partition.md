# Rosalind 3-Way Partition in COBOL

## Problem Understanding

3-Way Partition is a variation of the partitioning algorithm where an array is divided into three parts:
- Elements less than the pivot
- Elements equal to the pivot  
- Elements greater than the pivot

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. THREE_WAY_PARTITION.
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
       01 INPUT-RECORD.
          05 ARRAY-SIZE     PIC 9(4).
          05 ARRAY-ELEMENTS PIC 9(4) OCCURS 100 TIMES.
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
          05 RESULT-ELEMENTS PIC 9(4) OCCURS 100 TIMES.

       WORKING-STORAGE SECTION.
       01 I                    PIC 9(4) VALUE 1.
       01 J                    PIC 9(4) VALUE 1.
       01 K                    PIC 9(4) VALUE 1.
       01 PIVOT                PIC 9(4).
       01 TEMP                 PIC 9(4).
       01 SWAP-FLAG            PIC 9 VALUE 0.
       01 ARRAY-LENGTH         PIC 9(4).
       01 ARRAY-START          PIC 9(4) VALUE 1.
       01 ARRAY-END            PIC 9(4).
       01 LT-INDEX             PIC 9(4) VALUE 1.
       01 EQ-INDEX             PIC 9(4) VALUE 1.
       01 GT-INDEX             PIC 9(4) VALUE 1.
       01 CURRENT-ELEMENT      PIC 9(4).
       01 INPUT-FILENAME       PIC X(50) VALUE 'input.txt'.
       01 OUTPUT-FILENAME      PIC X(50) VALUE 'output.txt'.
       01 EOF-FLAG             PIC 9 VALUE 0.
       01 FILE-STATUS          PIC XX.

       01 INPUT-ARRAY.
          05 INPUT-ELEMENTS PIC 9(4) OCCURS 100 TIMES.

       01 OUTPUT-ARRAY.
          05 OUTPUT-ELEMENTS PIC 9(4) OCCURS 100 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           
           READ INPUT-FILE INTO INPUT-RECORD
               AT END MOVE 1 TO EOF-FLAG
           END-READ
           
           IF EOF-FLAG = 0
               MOVE ARRAY-SIZE TO ARRAY-LENGTH
               MOVE ARRAY-SIZE TO ARRAY-END
               MOVE 1 TO LT-INDEX
               MOVE 1 TO EQ-INDEX
               MOVE ARRAY-LENGTH TO GT-INDEX
               
               PERFORM 3-WAY-PARTITION
               
               PERFORM WRITE-OUTPUT
           END-IF
           
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       3-WAY-PARTITION.
           MOVE ARRAY-ELEMENTS(1) TO PIVOT
           
           PERFORM UNTIL EQ-INDEX > GT-INDEX
               IF INPUT-ELEMENTS(EQ-INDEX) < PIVOT
                   MOVE INPUT-ELEMENTS(EQ-INDEX) TO TEMP
                   MOVE INPUT-ELEMENTS(LT-INDEX) TO INPUT-ELEMENTS(EQ-INDEX)
                   MOVE TEMP TO INPUT-ELEMENTS(LT-INDEX)
                   ADD 1 TO LT-INDEX
                   ADD 1 TO EQ-INDEX
               ELSE IF INPUT-ELEMENTS(EQ-INDEX) > PIVOT
                   MOVE INPUT-ELEMENTS(GT-INDEX) TO TEMP
                   MOVE INPUT-ELEMENTS(EQ-INDEX) TO INPUT-ELEMENTS(GT-INDEX)
                   MOVE TEMP TO INPUT-ELEMENTS(EQ-INDEX)
                   SUBTRACT 1 FROM GT-INDEX
               ELSE
                   ADD 1 TO EQ-INDEX
               END-IF
           END-PERFORM.

       WRITE-OUTPUT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-LENGTH
               MOVE INPUT-ELEMENTS(I) TO OUTPUT-ELEMENTS(I)
           END-PERFORM
           
           WRITE OUTPUT-RECORD FROM OUTPUT-RECORD
               AFTER ADVANCING 1 LINE.

       END PROGRAM THREE_WAY_PARTITION.
```

## Explanation

This COBOL program implements the 3-Way Partition algorithm using the Dutch National Flag approach:

1. **Initialization**: 
   - Reads input array size and elements
   - Sets up three pointers: LT-INDEX (less than), EQ-INDEX (equal), and GT-INDEX (greater than)
   - Uses first element as pivot

2. **Partitioning Logic**:
   - Elements less than pivot are moved to the left (LT-INDEX area)
   - Elements equal to pivot remain in the middle
   - Elements greater than pivot are moved to the right (GT-INDEX area)
   - The algorithm uses three-way comparison and swapping

3. **Key Variables**:
   - `LT-INDEX`: Points to the end of elements less than pivot
   - `EQ-INDEX`: Current element being processed
   - `GT-INDEX`: Points to the start of elements greater than pivot
   - `PIVOT`: The value used for partitioning

4. **Algorithm Steps**:
   - Compare current element with pivot
   - If less: swap with left region and advance both pointers
   - If greater: swap with right region and move right pointer back
   - If equal: advance current pointer

## Sample Input Format
```
5
4 5 1 3 2
```

## Sample Output Format
```
1 2 3 4 5
```

The program handles the 3-Way Partition efficiently with O(n) time complexity and O(1) space complexity.

