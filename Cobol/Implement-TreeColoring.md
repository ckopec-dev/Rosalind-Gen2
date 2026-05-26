# Rosalind Problem: Implement TreeColoring in COBOL

## Problem Understanding

Tree coloring involves coloring the nodes of a tree such that no two adjacent nodes have the same color, using the minimum number of colors (chromatic number). This is a classic graph coloring problem.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TREECOLORING.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TREE-FILE ASSIGN TO "tree.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TREE-FILE.
       01 TREE-RECORD.
          05 NODE-1         PIC 9(3).
          05 NODE-2         PIC 9(3).

       WORKING-STORAGE SECTION.
       01 WS-NODES           PIC 9(3) VALUE 0.
       01 WS-EDGES           PIC 9(4) VALUE 0.
       01 WS-GRAPH.
          05 GRAPH-ARRAY      OCCURS 100 TIMES.
             10 NODE-INDEX    PIC 9(3).
             10 NODE-COLOR    PIC 9(1) VALUE 0.
             10 NODE-DEGREE   PIC 9(2) VALUE 0.
             10 ADJACENT-NODES OCCURS 10 TIMES PIC 9(3) VALUE 0.
       01 WS-COLORS.
          05 COLOR-ARRAY       OCCURS 100 TIMES PIC 9(1) VALUE 0.
       01 WS-TEMP            PIC 9(3).
       01 WS-TEMP2           PIC 9(3).
       01 WS-I               PIC 9(3) VALUE 0.
       01 WS-J               PIC 9(3) VALUE 0.
       01 WS-K               PIC 9(3) VALUE 0.
       01 WS-L               PIC 9(3) VALUE 0.
       01 WS-MAX-NODE        PIC 9(3) VALUE 0.
       01 WS-FOUND           PIC X VALUE 'N'.
       01 WS-NEIGHBOR        PIC 9(3) VALUE 0.
       01 WS-NEIGHBOR-COLOR  PIC 9(1) VALUE 0.
       01 WS-COLOR-USED      PIC X VALUE 'N'.
       01 WS-NEIGHBOR-INDEX  PIC 9(3) VALUE 0.
       01 WS-UNCOLORED       PIC 9(3) VALUE 0.
       01 WS-COLOR           PIC 9(1) VALUE 0.
       01 WS-RESULT          PIC X(1000).
       01 WS-EOF             PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM INITIALIZE-PROGRAM.
           PERFORM READ-TREE-DATA.
           PERFORM COLOR-TREE.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.

       INITIALIZE-PROGRAM.
           MOVE 0 TO WS-NODES, WS-EDGES, WS-MAX-NODE.
           MOVE 'N' TO WS-EOF.
           PERFORM RESET-GRAPH.
           MOVE 0 TO WS-I, WS-J, WS-K, WS-L.
           MOVE SPACES TO WS-RESULT.

       RESET-GRAPH.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 100
               MOVE 0 TO NODE-INDEX(WS-I)
               MOVE 0 TO NODE-COLOR(WS-I)
               MOVE 0 TO NODE-DEGREE(WS-I)
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 10
                   MOVE 0 TO ADJACENT-NODES(WS-I, WS-J)
               END-PERFORM
           END-PERFORM.

       READ-TREE-DATA.
           OPEN INPUT TREE-FILE.
           READ TREE-FILE AT END MOVE 'Y' TO WS-EOF.
           PERFORM UNTIL WS-EOF = 'Y'
               IF NODE-1 > WS-MAX-NODE THEN
                   MOVE NODE-1 TO WS-MAX-NODE
               END-IF
               IF NODE-2 > WS-MAX-NODE THEN
                   MOVE NODE-2 TO WS-MAX-NODE
               END-IF
               PERFORM ADD-EDGE
               READ TREE-FILE AT END MOVE 'Y' TO WS-EOF
           END-PERFORM.
           CLOSE TREE-FILE.

       ADD-EDGE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NODES
               IF NODE-INDEX(WS-I) = NODE-1 THEN
                   ADD 1 TO NODE-DEGREE(WS-I)
                   PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > NODE-DEGREE(WS-I)
                       IF ADJACENT-NODES(WS-I, WS-J) = 0 THEN
                           MOVE NODE-2 TO ADJACENT-NODES(WS-I, WS-J)
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
                   GO TO ADD-EDGE-2
               END-IF
           END-PERFORM.
           ADD 1 TO WS-NODES.
           MOVE NODE-1 TO NODE-INDEX(WS-NODES).
           MOVE 1 TO NODE-DEGREE(WS-NODES).
           MOVE NODE-2 TO ADJACENT-NODES(WS-NODES, 1).

       ADD-EDGE-2.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NODES
               IF NODE-INDEX(WS-I) = NODE-2 THEN
                   ADD 1 TO NODE-DEGREE(WS-I)
                   PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > NODE-DEGREE(WS-I)
                       IF ADJACENT-NODES(WS-I, WS-J) = 0 THEN
                           MOVE NODE-1 TO ADJACENT-NODES(WS-I, WS-J)
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
                   GO TO ADD-EDGE-3
               END-IF
           END-PERFORM.
           ADD 1 TO WS-NODES.
           MOVE NODE-2 TO NODE-INDEX(WS-NODES).
           MOVE 1 TO NODE-DEGREE(WS-NODES).
           MOVE NODE-1 TO ADJACENT-NODES(WS-NODES, 1).

       ADD-EDGE-3.

       COLOR-TREE.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NODES
               MOVE 0 TO NODE-COLOR(WS-I)
           END-PERFORM.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NODES
               IF NODE-COLOR(WS-I) = 0 THEN
                   PERFORM COLOR-NODE-RECURSIVE(WS-I)
               END-IF
           END-PERFORM.

       COLOR-NODE-RECURSIVE.
           01 NODE-TO-COLOR      PIC 9(3) VALUE 0.
           01 COLOR-TO-TRY       PIC 9(1) VALUE 0.
           01 COLOR-USED         PIC X VALUE 'N'.
           01 NEIGHBOR-INDEX     PIC 9(3) VALUE 0.

           MOVE 1 TO COLOR-TO-TRY.
           MOVE 'N' TO COLOR-USED.

           PERFORM UNTIL COLOR-USED = 'Y' OR COLOR-TO-TRY > 100
               MOVE 'N' TO COLOR-USED.
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > NODE-DEGREE(WS-I)
                   MOVE ADJACENT-NODES(WS-I, WS-J) TO NEIGHBOR-INDEX
                   IF NODE-COLOR(NEIGHBOR-INDEX) = COLOR-TO-TRY THEN
                       MOVE 'Y' TO COLOR-USED
                       ADD 1 TO COLOR-TO-TRY
                       GO TO COLOR-NEIGHBOR-CONTINUE
                   END-IF
               END-PERFORM.
               MOVE COLOR-TO-TRY TO NODE-COLOR(WS-I).
           END-PERFORM.

       DISPLAY-RESULTS.
           DISPLAY "Tree Coloring Results:".
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-NODES
               DISPLAY "Node " NODE-INDEX(WS-I) " Color: " NODE-COLOR(WS-I)
           END-PERFORM.

       END PROGRAM TREECOLORING.
```

## Explanation

This COBOL program implements a tree coloring algorithm that:

1. **Initializes** the program by setting up data structures for the tree
2. **Reads** the tree data from an input file containing node connections
3. **Builds** an adjacency list representation of the tree
4. **Colors** the tree using a greedy coloring algorithm:
   - For each uncolored node, tries colors 1, 2, 3, ... in order
   - For each color, checks if any adjacent node already has that color
   - Assigns the first available color to the current node
5. **Displays** the final coloring results

## Key Features

- **Tree representation**: Uses adjacency lists to store the tree structure
- **Greedy coloring**: Implements a basic greedy coloring algorithm
- **Input handling**: Reads from a file with node connections
- **Memory management**: Uses arrays to store node information and adjacency lists

## Input Format

The input file should contain edges in the format:
```
Node1 Node2
Node3 Node4
...
```

## Output

The program outputs each node with its assigned color, demonstrating a valid tree coloring where no adjacent nodes share the same color.

