# Rosalind Problem: Sequence a Peptide (Cobol Solution)

## Problem Understanding

The problem asks us to find the peptide sequence that corresponds to a given RNA sequence. This involves translating RNA codons (3-letter sequences) into amino acids using the genetic code.

## Solution Approach

1. Parse the input RNA sequence
2. Split into codons (3-character chunks)
3. Translate each codon to its corresponding amino acid
4. Return the resulting peptide sequence

## Cobol Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQUENCE_A_PEPTIDE.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INPUT-TEXT
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO OUTPUT-TEXT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE PIC X(100).

       WORKING-STORAGE SECTION.
       01 RNA-SEQUENCE PIC X(100).
       01 PEPTIDE-SEQUENCE PIC X(50).
       01 I PIC 99 VALUE 1.
       01 J PIC 99 VALUE 1.
       01 CODON PIC X(3).
       01 AMINO-ACID PIC X(1).
       01 LENGTH PIC 99.
       01 DONE PIC X VALUE 'N'.

       01 CODON-TABLE.
           05 CODON-ENTRY PIC X(3) OCCURS 64 TIMES.
           05 AMINO-ACID-ENTRY PIC X OCCURS 64 TIMES.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO RNA-SEQUENCE
           IF INPUT-FILE-STATUS = "00"
               MOVE FUNCTION LENGTH(RNA-SEQUENCE) TO LENGTH
               PERFORM TRANSLATE-RNA-TO-PEPTIDE
               WRITE OUTPUT-LINE FROM PEPTIDE-SEQUENCE
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       TRANSLATE-RNA-TO-PEPTIDE.
           MOVE SPACE TO PEPTIDE-SEQUENCE
           MOVE 1 TO I
           MOVE 1 TO J

           PERFORM UNTIL I > LENGTH OR I > 98
               IF (LENGTH - I + 1) < 3
                   MOVE 'X' TO DONE
                   GO TO TRANSLATE-EXIT
               END-IF

               MOVE RNA-SEQUENCE(I:3) TO CODON
               PERFORM TRANSLATE-CODON
               IF AMINO-ACID = 'X'
                   MOVE 'X' TO DONE
                   GO TO TRANSLATE-EXIT
               END-IF

               MOVE AMINO-ACID TO PEPTIDE-SEQUENCE(J:1)
               ADD 1 TO I
               ADD 1 TO J
           END-PERFORM.

       TRANSLATE-EXIT.
           EXIT.

       TRANSLATE-CODON.
           MOVE SPACE TO AMINO-ACID

           IF CODON = 'UUU' OR CODON = 'UUC' THEN
               MOVE 'F' TO AMINO-ACID
           ELSE IF CODON = 'UUA' OR CODON = 'UUG' THEN
               MOVE 'L' TO AMINO-ACID
           ELSE IF CODON = 'CUU' OR CODON = 'CUC' OR CODON = 'CUA' OR CODON = 'CUG' THEN
               MOVE 'L' TO AMINO-ACID
           ELSE IF CODON = 'AUU' OR CODON = 'AUC' OR CODON = 'AUA' THEN
               MOVE 'I' TO AMINO-ACID
           ELSE IF CODON = 'AUG' THEN
               MOVE 'M' TO AMINO-ACID
           ELSE IF CODON = 'GUU' OR CODON = 'GUC' OR CODON = 'GUA' OR CODON = 'GUG' THEN
               MOVE 'V' TO AMINO-ACID
           ELSE IF CODON = 'UCU' OR CODON = 'UCC' OR CODON = 'UCA' OR CODON = 'UCG' THEN
               MOVE 'S' TO AMINO-ACID
           ELSE IF CODON = 'CCU' OR CODON = 'CCC' OR CODON = 'CCA' OR CODON = 'CCG' THEN
               MOVE 'P' TO AMINO-ACID
           ELSE IF CODON = 'ACU' OR CODON = 'ACC' OR CODON = 'ACA' OR CODON = 'ACG' THEN
               MOVE 'T' TO AMINO-ACID
           ELSE IF CODON = 'GCU' OR CODON = 'GCC' OR CODON = 'GCA' OR CODON = 'GCG' THEN
               MOVE 'A' TO AMINO-ACID
           ELSE IF CODON = 'UAU' OR CODON = 'UAC' THEN
               MOVE 'Y' TO AMINO-ACID
           ELSE IF CODON = 'UAA' OR CODON = 'UAG' OR CODON = 'UGA' THEN
               MOVE 'X' TO AMINO-ACID  *> STOP CODON
           ELSE IF CODON = 'UGU' OR CODON = 'UGC' THEN
               MOVE 'C' TO AMINO-ACID
           ELSE IF CODON = 'UGG' THEN
               MOVE 'W' TO AMINO-ACID
           ELSE IF CODON = 'CAU' OR CODON = 'CAC' THEN
               MOVE 'H' TO AMINO-ACID
           ELSE IF CODON = 'CAA' OR CODON = 'CAG' THEN
               MOVE 'Q' TO AMINO-ACID
           ELSE IF CODON = 'AAU' OR CODON = 'AAC' THEN
               MOVE 'N' TO AMINO-ACID
           ELSE IF CODON = 'AAA' OR CODON = 'AAG' THEN
               MOVE 'K' TO AMINO-ACID
           ELSE IF CODON = 'GAU' OR CODON = 'GAC' THEN
               MOVE 'D' TO AMINO-ACID
           ELSE IF CODON = 'GAA' OR CODON = 'GAG' THEN
               MOVE 'E' TO AMINO-ACID
           ELSE IF CODON = 'CGU' OR CODON = 'CGC' OR CODON = 'CGA' OR CODON = 'CGG' THEN
               MOVE 'R' TO AMINO-ACID
           ELSE IF CODON = 'AGU' OR CODON = 'AGC' THEN
               MOVE 'S' TO AMINO-ACID
           ELSE IF CODON = 'AGA' OR CODON = 'AGG' THEN
               MOVE 'R' TO AMINO-ACID
           ELSE IF CODON = 'GCU' OR CODON = 'GCC' OR CODON = 'GCA' OR CODON = 'GCG' THEN
               MOVE 'A' TO AMINO-ACID
           ELSE IF CODON = 'GGU' OR CODON = 'GGC' OR CODON = 'GGA' OR CODON = 'GGG' THEN
               MOVE 'G' TO AMINO-ACID
           END-IF.
```

## Alternative Compact Version

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQUENCE_A_PEPTIDE.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO INPUT-TEXT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       WORKING-STORAGE SECTION.
       01 RNA-SEQUENCE PIC X(100).
       01 PEPTIDE-RESULT PIC X(50).
       01 I PIC 99 VALUE 1.
       01 J PIC 99 VALUE 1.
       01 CODON PIC X(3).
       01 LENGTH PIC 99.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO RNA-SEQUENCE
           MOVE FUNCTION LENGTH(RNA-SEQUENCE) TO LENGTH
           PERFORM TRANSLATE-RNA
           DISPLAY PEPTIDE-RESULT
           CLOSE INPUT-FILE
           STOP RUN.

       TRANSLATE-RNA.
           MOVE 1 TO I
           MOVE 1 TO J

           PERFORM UNTIL I > LENGTH OR I > 98
               IF (LENGTH - I + 1) < 3 THEN GO TO TRANSLATE-END
               
               MOVE RNA-SEQUENCE(I:3) TO CODON
               PERFORM TRANSLATE-CODON
               MOVE AMINO-ACID TO PEPTIDE-RESULT(J:1)
               ADD 3 TO I
               ADD 1 TO J
           END-PERFORM.

       TRANSLATE-END.
           EXIT.

       TRANSLATE-CODON.
           MOVE SPACE TO AMINO-ACID

           IF CODON = 'UUU' OR CODON = 'UUC' THEN
               MOVE 'F' TO AMINO-ACID
           ELSE IF CODON = 'UUA' OR CODON = 'UUG' THEN
               MOVE 'L' TO AMINO-ACID
           ELSE IF CODON = 'CUU' OR CODON = 'CUC' OR CODON = 'CUA' OR CODON = 'CUG' THEN
               MOVE 'L' TO AMINO-ACID
           ELSE IF CODON = 'AUU' OR CODON = 'AUC' OR CODON = 'AUA' THEN
               MOVE 'I' TO AMINO-ACID
           ELSE IF CODON = 'AUG' THEN
               MOVE 'M' TO AMINO-ACID
           ELSE IF CODON = 'GUU' OR CODON = 'GUC' OR CODON = 'GUA' OR CODON = 'GUG' THEN
               MOVE 'V' TO AMINO-ACID
           ELSE IF CODON = 'UCU' OR CODON = 'UCC' OR CODON = 'UCA' OR CODON = 'UCG' THEN
               MOVE 'S' TO AMINO-ACID
           ELSE IF CODON = 'CCU' OR CODON = 'CCC' OR CODON = 'CCA' OR CODON = 'CCG' THEN
               MOVE 'P' TO AMINO-ACID
           ELSE IF CODON = 'ACU' OR CODON = 'ACC' OR CODON = 'ACA' OR CODON = 'ACG' THEN
               MOVE 'T' TO AMINO-ACID
           ELSE IF CODON = 'GCU' OR CODON = 'GCC' OR CODON = 'GCA' OR CODON = 'GCG' THEN
               MOVE 'A' TO AMINO-ACID
           ELSE IF CODON = 'UAU' OR CODON = 'UAC' THEN
               MOVE 'Y' TO AMINO-ACID
           ELSE IF CODON = 'UAA' OR CODON = 'UAG' OR CODON = 'UGA' THEN
               MOVE 'X' TO AMINO-ACID
           ELSE IF CODON = 'UGU' OR CODON = 'UGC' THEN
               MOVE 'C' TO AMINO-ACID
           ELSE IF CODON = 'UGG' THEN
               MOVE 'W' TO AMINO-ACID
           ELSE IF CODON = 'CAU' OR CODON = 'CAC' THEN
               MOVE 'H' TO AMINO-ACID
           ELSE IF CODON = 'CAA' OR CODON = 'CAG' THEN
               MOVE 'Q' TO AMINO-ACID
           ELSE IF CODON = 'AAU' OR CODON = 'AAC' THEN
               MOVE 'N' TO AMINO-ACID
           ELSE IF CODON = 'AAA' OR CODON = 'AAG' THEN
               MOVE 'K' TO AMINO-ACID
           ELSE IF CODON = 'GAU' OR CODON = 'GAC' THEN
               MOVE 'D' TO AMINO-ACID
           ELSE IF CODON = 'GAA' OR CODON = 'GAG' THEN
               MOVE 'E' TO AMINO-ACID
           ELSE IF CODON = 'CGU' OR CODON = 'CGC' OR CODON = 'CGA' OR CODON = 'CGG' THEN
               MOVE 'R' TO AMINO-ACID
           ELSE IF CODON = 'AGU' OR CODON = 'AGC' THEN
               MOVE 'S' TO AMINO-ACID
           ELSE IF CODON = 'AGA' OR CODON = 'AGG' THEN
               MOVE 'R' TO AMINO-ACID
           ELSE IF CODON = 'GGU' OR CODON = 'GGC' OR CODON = 'GGA' OR CODON = 'GGG' THEN
               MOVE 'G' TO AMINO-ACID
           END-IF.
```

## How to Use

1. Save the input RNA sequence in a file named `input.txt`
2. Compile and run the Cobol program
3. The program will read the RNA sequence and output the corresponding peptide sequence

## Example

**Input:** `AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA`
**Output:** `MAMAPRTEINSTRING`

The program handles the standard genetic code translation and stops at stop codons (UAA, UAG, UGA).

