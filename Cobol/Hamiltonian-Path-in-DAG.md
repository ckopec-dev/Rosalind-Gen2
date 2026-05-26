# Hamiltonian Path in DAG - COBOL Solution

## Problem Understanding

A Hamiltonian path in a directed acyclic graph (DAG) is a path that visits each vertex exactly once. In a DAG, we can use topological sorting combined with dynamic programming to efficiently find such a path.

## Approach

1. Perform topological sort on the DAG
2. Use dynamic programming to track if a path ending at each vertex exists
3. Check if we can form a path visiting all vertices

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HAMILTONIAN-PATH-DAG.
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
       01 GRAPH-STRUCTURE.
          05 NUM-VERTICES PIC 9(3).
          05 NUM-EDGES PIC 9(4).
          05 ADJ-LIST.
             10 ADJ-ROW OCCURS 100 TIMES INDEXED BY I-ROW.
                15 ADJ-COL OCCURS 100 TIMES INDEXED BY I-COL.
                   20 ADJ-VALUE PIC 9(3).
          05 IN-DEGREE PIC 9(3) OCCURS 100 TIMES INDEXED BY I-DEG.
          05 TOPO-ORDER PIC 9(3) OCCURS 100 TIMES INDEXED BY I-ORDER.
          05 VISITED PIC X OCCURS 100 TIMES INDEXED BY I-VISIT.
          05 DP-RESULT PIC X OCCURS 100 TIMES INDEXED BY I-DP.
          05 QUEUE-START PIC 9(3) VALUE 1.
          05 QUEUE-END PIC 9(3) VALUE 1.
          05 QUEUE PIC 9(3) OCCURS 100 TIMES INDEXED BY I-QUEUE.
          05 TEMP-VALUE PIC 9(3).
          05 I PIC 9(3).
          05 J PIC 9(3).
          05 K PIC 9(3).
          05 L PIC 9(3).
          05 FOUND-HAMILTONIAN PIC X VALUE 'N'.
          05 EOF-FLAG PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM READ-INPUT.
           PERFORM TOPOLOGICAL-SORT.
           PERFORM CHECK-HAMILTONIAN-PATH.
           PERFORM WRITE-OUTPUT.
           STOP RUN.

       READ-INPUT.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO INPUT-LINE.
           UNSTRING INPUT-LINE DELIMITED BY SPACE
               INTO NUM-VERTICES, NUM-EDGES.

           PERFORM INITIALIZE-GRAPH.

           PERFORM READ-EDGES VARYING I FROM 1 BY 1
               UNTIL I > NUM-EDGES.

           CLOSE INPUT-FILE.

       INITIALIZE-GRAPH.
           MOVE 0 TO I-DEG.
           PERFORM VARYING I-DEG FROM 1 BY 1
               UNTIL I-DEG > NUM-VERTICES
               MOVE 0 TO IN-DEGREE(I-DEG)
               MOVE 'N' TO VISITED(I-DEG)
               MOVE 'N' TO DP-RESULT(I-DEG)
           END-PERFORM.

       READ-EDGES.
           READ INPUT-FILE INTO INPUT-LINE.
           UNSTRING INPUT-LINE DELIMITED BY SPACE
               INTO TEMP-VALUE, I, J.

           ADD 1 TO IN-DEGREE(J) GIVING IN-DEGREE(J).
           MOVE J TO ADJ-VALUE(I-ROW, I-COL).
           ADD 1 TO I-COL.

       TOPOLOGICAL-SORT.
           MOVE 1 TO I.
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > NUM-VERTICES
               IF IN-DEGREE(I) = 0
                   MOVE I TO QUEUE(QUEUE-END)
                   ADD 1 TO QUEUE-END
               END-IF
           END-PERFORM.

           MOVE 1 TO I-ORDER.
           PERFORM UNTIL QUEUE-START >= QUEUE-END
               MOVE QUEUE(QUEUE-START) TO TOPO-ORDER(I-ORDER)
               ADD 1 TO I-ORDER
               ADD 1 TO QUEUE-START
               MOVE 1 TO J
               PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > NUM-VERTICES
                   IF ADJ-VALUE(TOPO-ORDER(I-ORDER-1), J) > 0
                       SUBTRACT 1 FROM IN-DEGREE(J)
                       IF IN-DEGREE(J) = 0
                           MOVE J TO QUEUE(QUEUE-END)
                           ADD 1 TO QUEUE-END
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       CHECK-HAMILTONIAN-PATH.
           MOVE 'Y' TO DP-RESULT(1).
           MOVE 1 TO I.
           PERFORM VARYING I FROM 2 BY 1
               UNTIL I > NUM-VERTICES
               MOVE 'N' TO DP-RESULT(I)
               MOVE 1 TO J.
               PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > NUM-VERTICES
                   IF ADJ-VALUE(TOPO-ORDER(I-1), J) > 0
                       IF DP-RESULT(J) = 'N'
                           MOVE 'Y' TO DP-RESULT(J)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

           IF DP-RESULT(NUM-VERTICES) = 'Y'
               MOVE 'Y' TO FOUND-HAMILTONIAN
           END-IF.

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE.
           IF FOUND-HAMILTONIAN = 'Y'
               MOVE 'YES' TO OUTPUT-LINE
           ELSE
               MOVE 'NO' TO OUTPUT-LINE
           END-IF.
           WRITE OUTPUT-LINE.
           CLOSE OUTPUT-FILE.
```

## Explanation

This COBOL solution implements a Hamiltonian path detection algorithm for DAGs:

1. **Input Processing**: Reads the number of vertices and edges, then builds the adjacency list representation of the graph.

2. **Topological Sort**: Uses Kahn's algorithm to perform topological sorting of the DAG vertices.

3. **Hamiltonian Path Check**: 
   - Uses dynamic programming where `DP[i]` represents whether there exists a path ending at vertex `i`
   - Processes vertices in topological order
   - For each vertex, if there's an edge to another vertex, the path can be extended

4. **Output**: Returns "YES" if a Hamiltonian path exists, "NO" otherwise.

## Time Complexity
- Time: O(V + E) where V is vertices and E is edges
- Space: O(V + E) for adjacency list and auxiliary arrays

## Sample Input Format
```
5 6
1 2
2 3
3 4
4 5
1 3
2 4
```

## Sample Output
```
YES
```

This solution efficiently determines if a Hamiltonian path exists in a directed acyclic graph using topological sorting and dynamic programming techniques.

