# Rosalind Problem: Testing Acyclicity - Fortran Solution

## Problem Understanding

The problem asks us to determine whether a given directed graph is acyclic (i.e., contains no cycles). This is equivalent to checking if the graph is a Directed Acyclic Graph (DAG).

## Approach

I'll use a topological sorting approach with Kahn's algorithm:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. Process vertices from the queue, reducing in-degrees of neighbors
4. If we process all vertices, the graph is acyclic; otherwise, it contains cycles

## Fortran Implementation

```fortran
program testing_acyclicity
    implicit none
    integer, parameter :: max_nodes = 10000
    integer :: n, m, i, j, u, v, processed
    integer :: in_degree(max_nodes)
    integer :: queue(max_nodes)
    integer :: front, rear
    integer :: adj_list(max_nodes, max_nodes)
    integer :: adj_count(max_nodes)
    logical :: visited(max_nodes)
    
    ! Read input
    read(*,*) n, m
    
    ! Initialize arrays
    in_degree = 0
    adj_count = 0
    visited = .false.
    
    ! Read edges and build adjacency list
    do i = 1, m
        read(*,*) u, v
        adj_list(u, adj_count(u) + 1) = v
        adj_count(u) = adj_count(u) + 1
        in_degree(v) = in_degree(v) + 1
    end do
    
    ! Initialize queue with vertices having in-degree 0
    front = 1
    rear = 0
    do i = 1, n
        if (in_degree(i) == 0) then
            rear = rear + 1
            queue(rear) = i
        end if
    end do
    
    ! Process vertices using Kahn's algorithm
    processed = 0
    do while (front <= rear)
        u = queue(front)
        front = front + 1
        processed = processed + 1
        
        ! Process neighbors
        do i = 1, adj_count(u)
            v = adj_list(u, i)
            in_degree(v) = in_degree(v) - 1
            if (in_degree(v) == 0) then
                rear = rear + 1
                queue(rear) = v
            end if
        end do
    end do
    
    ! Check if all vertices were processed
    if (processed == n) then
        write(*,*) 0  ! Acyclic
    else
        write(*,*) 1  ! Contains cycle
    end if
    
end program testing_acyclicity
```

## Explanation

1. **Input Reading**: Read the number of vertices `n` and edges `m`, then read all edges.

2. **Data Structures**:
   - `in_degree`: Array to store in-degrees of each vertex
   - `adj_list`: Adjacency list representation of the graph
   - `adj_count`: Count of neighbors for each vertex
   - `queue`: Queue for BFS-like processing

3. **Algorithm Steps**:
   - Initialize in-degrees and adjacency list
   - Add all vertices with in-degree 0 to the queue
   - Process vertices from queue, reducing in-degrees of neighbors
   - If we process all vertices, the graph is acyclic (return 0)
   - If not all vertices are processed, there's a cycle (return 1)

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V + E) for storing the graph and auxiliary arrays

## Sample Input/Output

**Input:**
```
4 4
1 2
2 3
3 1
4 1
```

**Output:**
```
1
```

**Input:**
```
4 3
1 2
2 3
3 4
```

**Output:**
```
0
```

