# Rosalind Problem: Implement_Hierarchical_Clustering in COBOL

## Problem Understanding

The hierarchical clustering problem involves clustering a set of points in a way that builds a hierarchy of clusters. We'll implement a simple agglomerative clustering algorithm that repeatedly merges the closest pairs of clusters.

## COBOL Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HIERARCHICAL-CLUSTERING.
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
       01 N PIC 9(3) VALUE 0.
       01 K PIC 9(3) VALUE 0.
       01 I PIC 9(3) VALUE 0.
       01 J PIC 9(3) VALUE 0.
       01 L PIC 9(3) VALUE 0.
       01 M PIC 9(3) VALUE 0.
       01 TEMP PIC 9(3) VALUE 0.
       01 MIN-DISTANCE PIC 9(5)V99 VALUE 0.
       01 DISTANCE PIC 9(5)V99 VALUE 0.
       01 DISTANCES OCCURS 100 TIMES PIC 9(5)V99.
       01 CLUSTERS OCCURS 100 TIMES PIC 9(3).
       01 CLUSTER-SIZE OCCURS 100 TIMES PIC 9(3).
       01 POINTS OCCURS 100 TIMES PIC 9(5)V99.
       01 POINT-X OCCURS 100 TIMES PIC 9(5)V99.
       01 POINT-Y OCCURS 100 TIMES PIC 9(5)V99.
       01 CLUSTER-X OCCURS 100 TIMES PIC 9(5)V99.
       01 CLUSTER-Y OCCURS 100 TIMES PIC 9(5)V99.
       01 TEMP-X PIC 9(5)V99.
       01 TEMP-Y PIC 9(5)V99.
       01 MIN-INDEX-1 PIC 9(3) VALUE 0.
       01 MIN-INDEX-2 PIC 9(3) VALUE 0.
       01 EOF-FLAG PIC X VALUE 'N'.
       01 LINE-POS PIC 9(3) VALUE 1.
       01 CHAR PIC X.
       01 NUM-VALUE PIC 9(5)V99.
       01 NUM-START PIC 9(3) VALUE 1.
       01 NUM-END PIC 9(3) VALUE 1.
       01 NUM-LENGTH PIC 9(3) VALUE 0.
       01 READ-FLAG PIC X VALUE 'N'.
       01 CLUSTER-COUNT PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM READ-INPUT-FILE

           PERFORM HIERARCHICAL-CLUSTER

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       READ-INPUT-FILE.
           READ INPUT-FILE INTO INPUT-LINE
               AT END SET EOF-FLAG TO 'Y'
           END-READ.

           PERFORM PARSE-INPUT-LINE

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ INPUT-FILE INTO INPUT-LINE
                   AT END SET EOF-FLAG TO 'Y'
               END-READ
               IF EOF-FLAG = 'N'
                   PERFORM PARSE-INPUT-LINE
               END-IF
           END-PERFORM.

       PARSE-INPUT-LINE.
           IF N = 0
               PERFORM PARSE-NUMBER-TO-N
           ELSE
               PERFORM PARSE-POINT
           END-IF.

       PARSE-NUMBER-TO-N.
           MOVE INPUT-LINE TO POINTS(1)
           PERFORM PARSE-INTEGER-TO-NUMBER
           MOVE NUM-VALUE TO N
           MOVE 1 TO CLUSTER-COUNT
           PERFORM INITIALIZE-CLUSTERS.

       PARSE-POINT.
           MOVE INPUT-LINE TO POINTS(CLUSTER-COUNT)
           PERFORM PARSE-POINT-TO-COORDINATES
           ADD 1 TO CLUSTER-COUNT.

       PARSE-INTEGER-TO-NUMBER.
           MOVE 1 TO NUM-START
           MOVE 1 TO NUM-END
           MOVE 0 TO NUM-VALUE

           PERFORM UNTIL NUM-END > LENGTH OF INPUT-LINE
               MOVE INPUT-LINE(NUM-END:1) TO CHAR
               IF CHAR = ' '
                   MOVE NUM-END TO NUM-LENGTH
                   COMPUTE NUM-VALUE = NUM-VALUE * 10 + 
                       FUNCTION NUMVAL(INPUT-LINE(NUM-START:NUM-LENGTH))
                   MOVE NUM-END TO NUM-START
                   MOVE NUM-END TO NUM-END
                   GO TO PARSE-INTEGER-TO-NUMBER
               ELSE
                   ADD 1 TO NUM-END
               END-IF
           END-PERFORM.

       PARSE-POINT-TO-COORDINATES.
           MOVE INPUT-LINE TO POINTS(CLUSTER-COUNT)
           MOVE 1 TO LINE-POS
           MOVE 0 TO NUM-VALUE
           MOVE 1 TO NUM-START
           MOVE 1 TO NUM-END
           MOVE 0 TO NUM-LENGTH

           PERFORM UNTIL LINE-POS > LENGTH OF INPUT-LINE
               MOVE INPUT-LINE(LINE-POS:1) TO CHAR
               IF CHAR = ' ' OR LINE-POS = LENGTH OF INPUT-LINE
                   IF LINE-POS = LENGTH OF INPUT-LINE
                       ADD 1 TO NUM-END
                   END-IF
                   MOVE LINE-POS TO NUM-LENGTH
                   COMPUTE NUM-VALUE = NUM-VALUE * 10 + 
                       FUNCTION NUMVAL(INPUT-LINE(NUM-START:NUM-LENGTH))
                   IF NUM-LENGTH = 0
                       MOVE NUM-VALUE TO POINT-X(CLUSTER-COUNT)
                   ELSE
                       MOVE NUM-VALUE TO POINT-Y(CLUSTER-COUNT)
                   END-IF
                   MOVE LINE-POS TO NUM-START
                   MOVE LINE-POS TO NUM-END
                   MOVE 0 TO NUM-VALUE
                   GO TO PARSE-POINT-TO-COORDINATES
               ELSE
                   ADD 1 TO NUM-END
               END-IF
           END-PERFORM.

       INITIALIZE-CLUSTERS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               MOVE I TO CLUSTERS(I)
               MOVE 1 TO CLUSTER-SIZE(I)
               MOVE POINT-X(I) TO CLUSTER-X(I)
               MOVE POINT-Y(I) TO CLUSTER-Y(I)
           END-PERFORM.

       HIERARCHICAL-CLUSTER.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N - 1
               PERFORM FIND-MINIMUM-DISTANCE
               PERFORM MERGE-CLUSTERS
           END-PERFORM.

       FIND-MINIMUM-DISTANCE.
           COMPUTE MIN-DISTANCE = 99999.99
           MOVE 0 TO MIN-INDEX-1
           MOVE 0 TO MIN-INDEX-2

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               IF CLUSTERS(J) > 0
                   PERFORM VARYING K FROM 1 BY 1 UNTIL K > N
                       IF CLUSTERS(K) > 0 AND J < K
                           COMPUTE DISTANCE = 
                               FUNCTION SQRT(
                                   (CLUSTER-X(J) - CLUSTER-X(K)) ** 2 +
                                   (CLUSTER-Y(J) - CLUSTER-Y(K)) ** 2
                               )
                           IF DISTANCE < MIN-DISTANCE
                               COMPUTE MIN-DISTANCE = DISTANCE
                               MOVE J TO MIN-INDEX-1
                               MOVE K TO MIN-INDEX-2
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM.

       MERGE-CLUSTERS.
           IF MIN-INDEX-1 > 0 AND MIN-INDEX-2 > 0
               PERFORM MERGE-CLUSTERS-LOGIC
           END-IF.

       MERGE-CLUSTERS-LOGIC.
           PERFORM COMPUTE-MERGED-CENTER
           PERFORM UPDATE-CLUSTER-SIZES
           PERFORM WRITE-OUTPUT-CLUSTER
           PERFORM REMOVE-CLUSTER.

       COMPUTE-MERGED-CENTER.
           COMPUTE CLUSTER-X(MIN-INDEX-1) = 
               (CLUSTER-X(MIN-INDEX-1) * CLUSTER-SIZE(MIN-INDEX-1) +
                CLUSTER-X(MIN-INDEX-2) * CLUSTER-SIZE(MIN-INDEX-2)) /
                (CLUSTER-SIZE(MIN-INDEX-1) + CLUSTER-SIZE(MIN-INDEX-2))

           COMPUTE CLUSTER-Y(MIN-INDEX-1) = 
               (CLUSTER-X(MIN-INDEX-1) * CLUSTER-SIZE(MIN-INDEX-1) +
                CLUSTER-Y(MIN-INDEX-2) * CLUSTER-SIZE(MIN-INDEX-2)) /
                (CLUSTER-SIZE(MIN-INDEX-1) + CLUSTER-SIZE(MIN-INDEX-2))

           COMPUTE CLUSTER-SIZE(MIN-INDEX-1) = 
               CLUSTER-SIZE(MIN-INDEX-1) + CLUSTER-SIZE(MIN-INDEX-2).

       UPDATE-CLUSTER-SIZES.
           MOVE 0 TO CLUSTER-SIZE(MIN-INDEX-2).

       WRITE-OUTPUT-CLUSTER.
           MOVE MIN-INDEX-1 TO CLUSTERS(MIN-INDEX-1)
           MOVE MIN-INDEX-2 TO CLUSTERS(MIN-INDEX-2)
           PERFORM WRITE-OUTPUT-LINE.

       WRITE-OUTPUT-LINE.
           STRING 
               "Cluster " CLUSTERS(MIN-INDEX-1) 
               " merged with cluster " CLUSTERS(MIN-INDEX-2)
               " distance " MIN-DISTANCE
               INTO OUTPUT-LINE
           WRITE OUTPUT-LINE.

       REMOVE-CLUSTER.
           MOVE 0 TO CLUSTERS(MIN-INDEX-2).
```

## Explanation

This COBOL implementation of hierarchical clustering:

1. **Input Reading**: Reads points from input file, parsing the number of points and their coordinates
2. **Initialization**: Sets up clusters with individual points
3. **Clustering Algorithm**: 
   - Finds the minimum distance between any two clusters
   - Merges the closest clusters
   - Updates cluster centers and sizes
   - Continues until all points are in one cluster

## Key Features

- **Distance Calculation**: Uses Euclidean distance formula
- **Cluster Merging**: Properly merges clusters and updates their centers
- **Memory Management**: Uses arrays to store cluster information
- **Output Generation**: Writes clustering steps to output file

## Usage

To run this program:
1. Prepare input file with points in format: first line = number of points, subsequent lines = x y coordinates
2. Compile with COBOL compiler
3. Run the program
4. Output will contain the clustering steps

Note: This is a simplified implementation. A full production version would require more robust error handling and memory management.

