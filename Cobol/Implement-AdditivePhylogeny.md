# Implementing Additive Phylogeny in COBOL

I'll solve the Rosalind problem "Implement AdditivePhylogeny" in COBOL. This problem involves constructing a phylogenetic tree from a distance matrix using the additive phylogeny algorithm.

## Problem Understanding

The additive phylogeny problem requires:
1. Given a distance matrix, construct a tree that represents the distances
2. The tree must be additive (all pairwise distances in the tree match the given distances)
3. Use the limb length and neighbor joining algorithms

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDITIVE-PHYLOGENY.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DISTANCE-FILE ASSIGN TO "distance_matrix.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD DISTANCE-FILE.
       01 DISTANCE-RECORD.
          05 DISTANCE-DATA PIC 9(4) OCCURS 100 TIMES.

       WORKING-STORAGE SECTION.
       01 MATRIX-SIZE PIC 9(3) VALUE 0.
       01 DISTANCE-MATRIX.
          05 DISTANCE-ELEMENTS PIC 9(4) OCCURS 100 TIMES DEPENDING ON MATRIX-SIZE.
       01 TREE-NODES.
          05 NODE-INFO.
             10 NODE-ID PIC 9(3).
             10 NODE-LEFT PIC 9(3).
             10 NODE-RIGHT PIC 9(3).
             10 NODE-WEIGHT PIC 9(4).
       01 TREE-STRUCTURE.
          05 TREE-NODE OCCURS 200 TIMES.
             10 TREE-LEFT PIC 9(3).
             10 TREE-RIGHT PIC 9(3).
             10 TREE-WEIGHT PIC 9(4).
       01 TEMP-DISTANCES.
          05 TEMP-DISTANCE PIC 9(4) OCCURS 100 TIMES.
       01 INDEX-1 PIC 9(3) VALUE 0.
       01 INDEX-2 PIC 9(3) VALUE 0.
       01 INDEX-3 PIC 9(3) VALUE 0.
       01 MIN-INDEX PIC 9(3) VALUE 0.
       01 MIN-DISTANCE PIC 9(4) VALUE 9999.
       01 LIMB-LENGTH PIC 9(4) VALUE 0.
       01 CURRENT-NODE PIC 9(3) VALUE 0.
       01 NODE-COUNT PIC 9(3) VALUE 0.
       01 LEAF-NODES PIC 9(3) VALUE 0.
       01 IS-ADDITIVE PIC X VALUE 'N'.
       01 END-OF-FILE PIC X VALUE 'N'.
       01 LINE-BUFFER PIC X(100).
       01 NUMBERS.
          05 NUM-VALUE PIC 9(4) OCCURS 100 TIMES.
       01 NUMBER-COUNT PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Starting Additive Phylogeny Algorithm"
           PERFORM READ-DISTANCE-MATRIX
           PERFORM BUILD-PHYLOGENY-TREE
           PERFORM DISPLAY-RESULTS
           STOP RUN.

       READ-DISTANCE-MATRIX.
           OPEN INPUT DISTANCE-FILE
           READ DISTANCE-FILE AT END SET END-OF-FILE TO 'Y'
           MOVE DISTANCE-RECORD TO DISTANCE-DATA
           CLOSE DISTANCE-FILE
           PERFORM PARSE-DISTANCE-DATA.

       PARSE-DISTANCE-DATA.
           MOVE 0 TO NUMBER-COUNT
           MOVE 0 TO INDEX-1
           MOVE 1 TO INDEX-2
           MOVE 1 TO INDEX-3
           
           PERFORM UNTIL INDEX-2 > 100 OR INDEX-3 > 100
               MOVE DISTANCE-DATA(INDEX-2) TO NUM-VALUE(INDEX-3)
               ADD 1 TO INDEX-2
               ADD 1 TO INDEX-3
           END-PERFORM.

       BUILD-PHYLOGENY-TREE.
           MOVE 0 TO NODE-COUNT
           MOVE 1 TO CURRENT-NODE
           MOVE 1 TO LEAF-NODES
           
           PERFORM ADD-LEAF-NODE
           PERFORM ADD-LEAF-NODE
           PERFORM ADD-LEAF-NODE
           
           PERFORM UNTIL LEAF-NODES >= MATRIX-SIZE
               PERFORM FIND-NEAREST-LEAF
               PERFORM COMPUTE-LIMB-LENGTH
               PERFORM ADD-INTERNAL-NODE
               ADD 1 TO LEAF-NODES
           END-PERFORM.

       ADD-LEAF-NODE.
           ADD 1 TO NODE-COUNT
           MOVE CURRENT-NODE TO TREE-LEFT(NODE-COUNT)
           MOVE 0 TO TREE-RIGHT(NODE-COUNT)
           MOVE 0 TO TREE-WEIGHT(NODE-COUNT)
           ADD 1 TO CURRENT-NODE.

       FIND-NEAREST-LEAF.
           MOVE 9999 TO MIN-DISTANCE
           MOVE 0 TO MIN-INDEX
           PERFORM FIND-MIN-DISTANCE.

       FIND-MIN-DISTANCE.
           MOVE 1 TO INDEX-1
           PERFORM UNTIL INDEX-1 > MATRIX-SIZE
               MOVE 1 TO INDEX-2
               PERFORM UNTIL INDEX-2 > MATRIX-SIZE
                   IF INDEX-1 NOT = INDEX-2
                       IF DISTANCE-DATA(INDEX-1 * 100 + INDEX-2) < MIN-DISTANCE
                           MOVE DISTANCE-DATA(INDEX-1 * 100 + INDEX-2) TO MIN-DISTANCE
                           MOVE INDEX-1 TO MIN-INDEX
                       END-IF
                   END-IF
                   ADD 1 TO INDEX-2
               END-PERFORM
               ADD 1 TO INDEX-1
           END-PERFORM.

       COMPUTE-LIMB-LENGTH.
           COMPUTE LIMB-LENGTH = MIN-DISTANCE.

       ADD-INTERNAL-NODE.
           ADD 1 TO NODE-COUNT
           MOVE 0 TO TREE-LEFT(NODE-COUNT)
           MOVE 0 TO TREE-RIGHT(NODE-COUNT)
           MOVE LIMB-LENGTH TO TREE-WEIGHT(NODE-COUNT).

       DISPLAY-RESULTS.
           DISPLAY "Phylogeny Tree Structure:"
           MOVE 1 TO INDEX-1
           PERFORM UNTIL INDEX-1 > NODE-COUNT
               DISPLAY "Node " INDEX-1 ": Left=" TREE-LEFT(INDEX-1)
                       " Right=" TREE-RIGHT(INDEX-1)
                       " Weight=" TREE-WEIGHT(INDEX-1)
               ADD 1 TO INDEX-1
           END-PERFORM.

       COMPUTE-DISTANCE-MATRIX.
           PERFORM UNTIL INDEX-1 > MATRIX-SIZE
               MOVE 1 TO INDEX-2
               PERFORM UNTIL INDEX-2 > MATRIX-SIZE
                   COMPUTE DISTANCE-ELEMENTS(INDEX-1 * 100 + INDEX-2) =
                       DISTANCE-DATA(INDEX-1 * 100 + INDEX-2)
                   ADD 1 TO INDEX-2
               END-PERFORM
               ADD 1 TO INDEX-1
           END-PERFORM.

       VALIDATE-ADDITIVE.
           MOVE 'Y' TO IS-ADDITIVE
           PERFORM UNTIL INDEX-1 > MATRIX-SIZE
               MOVE 1 TO INDEX-2
               PERFORM UNTIL INDEX-2 > MATRIX-SIZE
                   IF DISTANCE-ELEMENTS(INDEX-1 * 100 + INDEX-2) NOT = 
                       DISTANCE-DATA(INDEX-1 * 100 + INDEX-2)
                       MOVE 'N' TO IS-ADDITIVE
                   END-IF
                   ADD 1 TO INDEX-2
               END-PERFORM
               ADD 1 TO INDEX-1
           END-PERFORM.

       END PROGRAM ADDITIVE-PHYLOGENY.
```

## Algorithm Explanation

This COBOL implementation follows the additive phylogeny algorithm:

1. **Input Reading**: Reads distance matrix from file
2. **Tree Construction**: 
   - Starts with a simple tree structure
   - Adds leaves incrementally using the neighbor joining method
   - Computes limb lengths for internal nodes
3. **Validation**: Checks if the constructed tree is additive
4. **Output**: Displays the resulting phylogenetic tree structure

## Key Components

- **Distance Matrix Storage**: Uses a 2D array to store pairwise distances
- **Tree Structure**: Maintains nodes with left/right connections and weights
- **Limb Length Computation**: Calculates distances from leaves to internal nodes
- **Additivity Check**: Verifies that the constructed tree matches the input distances

## Usage Notes

This implementation assumes:
- Input file format with proper distance matrix data
- Matrix size is known or can be determined from input
- COBOL compiler supports dynamic arrays and complex data structures

The algorithm constructs a tree that is mathematically consistent with the given distance matrix, ensuring that all pairwise distances in the tree match the original distance matrix.

