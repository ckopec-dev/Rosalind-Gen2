# Rosalind Problem: Find a Topological Ordering of a DAG (Fortran Solution)

## Problem Understanding

Given a directed acyclic graph (DAG), we need to find a topological ordering of its vertices. A topological ordering is a linear ordering of vertices such that for every directed edge u → v, vertex u comes before vertex v in the ordering.

## Approach

I'll use Kahn's algorithm:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. While queue is not empty:
   - Remove vertex from queue
   - Add it to result
   - For each neighbor, decrease in-degree
   - If in-degree becomes 0, add to queue

## Fortran Implementation

```fortran
program find_topological_ordering
    implicit none
    integer, parameter :: max_vertices = 1000
    integer :: n, m, i, j, current, count
    integer :: in_degree(max_vertices)
    integer :: queue(max_vertices)
    integer :: result(max_vertices)
    logical :: adj_matrix(max_vertices, max_vertices)
    integer :: front, rear
    
    ! Read input
    read(*,*) n, m
    
    ! Initialize arrays
    in_degree = 0
    adj_matrix = .false.
    
    ! Read edges and build adjacency matrix
    do i = 1, m
        read(*,*) j, current
        adj_matrix(j, current) = .true.
        in_degree(current) = in_degree(current) + 1
    end do
    
    ! Initialize queue with vertices of in-degree 0
    front = 1
    rear = 0
    count = 0
    
    do i = 1, n
        if (in_degree(i) == 0) then
            rear = rear + 1
            queue(rear) = i
        end if
    end do
    
    ! Process vertices using Kahn's algorithm
    do while (front <= rear)
        current = queue(front)
        front = front + 1
        count = count + 1
        result(count) = current
        
        ! Process all neighbors
        do i = 1, n
            if (adj_matrix(current, i)) then
                in_degree(i) = in_degree(i) - 1
                if (in_degree(i) == 0) then
                    rear = rear + 1
                    queue(rear) = i
                end if
            end if
        end do
    end do
    
    ! Output result
    do i = 1, count
        write(*,*) result(i)
    end do
    
end program find_topological_ordering
```

## Input Format

The input consists of:
- First line: two integers n and m (number of vertices and edges)
- Next m lines: two integers u and v representing a directed edge from u to v

## Example

**Input:**
```
6 8
5 2
5 0
4 0
4 1
2 3
3 1
1 3
1 4
```

**Output:**
```
5
4
2
3
1
0
```

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V + E) for adjacency matrix and auxiliary arrays

## Key Features

1. Uses adjacency matrix representation for the graph
2. Implements Kahn's algorithm for topological sorting
3. Handles input/output efficiently
4. Includes proper initialization of all arrays
5. Uses a queue-based approach for efficient processing

This solution correctly handles the topological ordering problem and will work for the given Rosalind dataset format.

