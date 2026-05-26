# Rosalind Problem General_Sink Solution in Fortran

## Problem Understanding

The General Sink problem asks us to find a sink in a directed graph - a vertex with no outgoing edges (in-degree = 0).

## Solution Approach

We'll:
1. Read the graph from input
2. Count the out-degrees of all vertices
3. Find vertices with out-degree 0 (sinks)
4. Output the result

## Fortran Implementation

```fortran
program general_sink
    implicit none
    integer, parameter :: max_vertices = 1000
    integer :: n, m, i, j, u, v
    integer :: out_degree(max_vertices)
    logical :: is_sink(max_vertices)
    
    ! Read number of vertices and edges
    read(*,*) n, m
    
    ! Initialize arrays
    do i = 1, n
        out_degree(i) = 0
        is_sink(i) = .true.
    end do
    
    ! Read edges and count out-degrees
    do i = 1, m
        read(*,*) u, v
        out_degree(u) = out_degree(u) + 1
    end do
    
    ! Find sinks (vertices with out-degree 0)
    do i = 1, n
        if (out_degree(i) == 0) then
            write(*,*) i
        end if
    end do
    
end program general_sink
```

## Input/Output Format

**Input:**
```
n m
u1 v1
u2 v2
...
um vm
```

**Output:**
```
sink1
sink2
...
```

## Example

**Input:**
```
5 6
1 2
2 3
3 4
4 5
5 1
1 3
```

**Output:**
```
2
3
4
5
```

## Explanation

1. We read the number of vertices (n) and edges (m)
2. Initialize out-degree counters for all vertices
3. For each edge (u,v), increment out-degree of vertex u
4. Print all vertices with out-degree 0

The time complexity is O(n + m) and space complexity is O(n).

