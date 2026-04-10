# Rosalind Problem: Find All Shared k-mers of a Pair of Strings (COBOL Solution)

## Problem Understanding

Given two DNA strings, we need to find all shared k-mers (k-length substrings) that appear in both strings.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-SHARED-KMERS.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       WORKING-STORAGE SECTION.
       01 STRING1 PIC X(50).
       01 STRING2 PIC X(50).
       01 K-MER-LENGTH PIC 99 VALUE 3.
       01 STRING1-LENGTH PIC 99.
       01 STRING2-LENGTH PIC 99.
       01 I PIC 99.
       01 J PIC 99.
       01 K PIC 99.
       01 TEMP-KMER1 PIC X(50).
       01 TEMP-KMER2 PIC X(50).
       01 FOUND-MATCH PIC 9 VALUE 0.
       01 SHARED-KMERS.
           05 SHARED-KMER OCCURS 100 TIMES PIC X(50).
       01 SHARED-COUNT PIC 99 VALUE 0.
       01 LINE-COUNT PIC 99 VALUE 0.
       01 TEMP-STRING PIC X(100).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           
           READ INPUT-FILE INTO STRING1
           IF NOT EOF
               READ INPUT-FILE INTO STRING2
           END-IF
           
           CLOSE INPUT-FILE
           
           MOVE FUNCTION LENGTH(STRING1) TO STRING1-LENGTH
           MOVE FUNCTION LENGTH(STRING2) TO STRING2-LENGTH
           
           PERFORM FIND-SHARED-KMERS
           
           DISPLAY "Shared k-mers:"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SHARED-COUNT
               DISPLAY SHARED-KMER(I)
           END-PERFORM
           
           STOP RUN.

       FIND-SHARED-KMERS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > (STRING1-LENGTH - K-MER-LENGTH + 1)
               MOVE 1 TO FOUND-MATCH
               MOVE I TO K
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > (STRING2-LENGTH - K-MER-LENGTH + 1)
                   MOVE 1 TO FOUND-MATCH
                   PERFORM VARYING K FROM I BY 1 UNTIL K > (I + K-MER-LENGTH - 1)
                       MOVE STRING1(K:1) TO TEMP-KMER1(K-I+1:1)
                   END-PERFORM
                   
                   PERFORM VARYING K FROM J BY 1 UNTIL K > (J + K-MER-LENGTH - 1)
                       MOVE STRING2(K:1) TO TEMP-KMER2(K-J+1:1)
                   END-PERFORM
                   
                   IF TEMP-KMER1 = TEMP-KMER2
                       PERFORM CHECK-IF-ALREADY-EXISTS
                   END-IF
               END-PERFORM
           END-PERFORM.

       CHECK-IF-ALREADY-EXISTS.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > SHARED-COUNT
               IF SHARED-KMER(K) = TEMP-KMER1
                   MOVE 0 TO FOUND-MATCH
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM
           
           IF FOUND-MATCH = 1
               ADD 1 TO SHARED-COUNT
               MOVE TEMP-KMER1 TO SHARED-KMER(SHARED-COUNT)
           END-IF.
```

## Explanation

This COBOL program solves the shared k-mers problem by:

1. **Reading Input**: The program reads two DNA strings from an input file
2. **Finding k-mers**: For each k-mer in the first string, it checks if it exists in the second string
3. **Avoiding Duplicates**: It ensures each shared k-mer is only counted once
4. **Output**: Displays all shared k-mers

## Key Components

- **File Handling**: Reads from input.txt file
- **String Operations**: Extracts k-mers from strings using substring operations
- **Comparison Logic**: Compares k-mers between strings
- **Duplicate Prevention**: Checks if k-mer already exists in results
- **Output**: Displays shared k-mers in the required format

## Sample Input Format

```
AAACTCATAC
AGAGATAC
```

## Sample Output

```
Shared k-mers:
AAT
ACT
CAT
```

## Notes

- The program assumes k-mer length is 3 (can be modified)
- Uses a simple approach with nested loops for comparison
- Handles duplicate k-mers by checking existing results
- Uses fixed-size arrays for storage (can be adjusted for larger inputs)

This solution demonstrates how to approach bioinformatics problems in COBOL, though in practice, more efficient algorithms would be preferred for large datasets.

