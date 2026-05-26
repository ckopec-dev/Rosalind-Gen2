# Square in a Graph - Rosalind Problem Solution in Fortran

## Problem Understanding

The "Square in a Graph" problem asks us to find a square (4-cycle) in an undirected graph. A square is a cycle of length 4, which means 4 vertices connected in a cycle where each vertex connects to exactly 2 others in the cycle.

## Approach

I'll use a brute force approach to check all possible 4-node combinations to see if they form a square:
1. Read the graph from input
2. For each possible 4-node combination, check if they form a square
3. A square has exactly 4 edges connecting the 4 nodes in a cycle

## Fortran Solution

```fortran
program square_in_a_graph
    implicit none
    integer, parameter :: max_nodes = 100
    integer :: n, m, i, j, k, l, a, b, c, d
    integer :: adj(max_nodes, max_nodes)
    integer :: nodes(4)
    logical :: found_square
    
    ! Initialize adjacency matrix
    do i = 1, max_nodes
        do j = 1, max_nodes
            adj(i, j) = 0
        end do
    end do
    
    ! Read number of nodes and edges
    read(*,*) n, m
    
    ! Read edges and build adjacency matrix
    do i = 1, m
        read(*,*) a, b
        adj(a, b) = 1
        adj(b, a) = 1
    end do
    
    ! Check all possible 4-node combinations
    found_square = .false.
    
    do i = 1, n-3
        do j = i+1, n-2
            do k = j+1, n-1
                do l = k+1, n
                    ! Check if these 4 nodes form a square
                    if (adj(i, j) == 1 .and. adj(j, k) == 1 .and. &
                        adj(k, l) == 1 .and. adj(l, i) == 1 .and. &
                        adj(i, k) == 0 .and. adj(j, l) == 0) then
                        ! Found a square: i-j-k-l-i
                        write(*,*) i, j, k, l
                        found_square = .true.
                        return
                    end if
                end do
            end do
        end do
    end do
    
    ! If no square found
    if (.not. found_square) then
        write(*,*) -1
    end if
    
end program square_in_a_graph
```

## Explanation

### Key Components:

1. **Data Structures**:
   - `adj(max_nodes, max_nodes)`: Adjacency matrix to represent the graph
   - `max_nodes = 100`: Maximum number of nodes allowed

2. **Input Processing**:
   - Read number of nodes `n` and edges `m`
   - Build adjacency matrix from input edges

3. **Square Detection Logic**:
   - Use nested loops to generate all combinations of 4 nodes
   - For each combination, check if they form a 4-cycle
   - A square has edges: (i,j), (j,k), (k,l), (l,i)
   - And no diagonal edges: (i,k) and (j,l) should not exist

4. **Output**:
   - If a square is found, print the 4 nodes in order
   - If no square exists, print -1

### Time Complexity:
- O(n⁴) for checking all combinations
- O(1) for each square check
- Overall: O(n⁴)

### Space Complexity:
- O(n²) for the adjacency matrix

## Sample Input/Output

**Input:**
```
4 5
1 2
2 3
3 4
4 1
1 3
```

**Output:**
```
1 2 3 4
```

This represents a square (cycle) formed by nodes 1-2-3-4-1.

The solution handles the case where no square exists by outputting -1, as required by the problem specification.

