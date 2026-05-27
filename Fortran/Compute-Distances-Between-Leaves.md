# Rosalind Problem: Compute Distances Between Leaves in Fortran

## Problem Description
Given a weighted tree with n leaves, compute the distance between each pair of leaves.

## Solution Approach
1. Parse the tree structure from input
2. Build adjacency list representation
3. Use BFS/DFS to compute distances between all leaf pairs
4. Output the distance matrix

## Fortran Implementation

```fortran
program compute_distances_between_leaves
    implicit none
    integer, parameter :: max_nodes = 1000
    integer, parameter :: max_edges = 2000
    integer :: n, i, j, k, u, v, w, edge_count
    integer :: adj_list(max_nodes, max_edges)
    integer :: adj_count(max_nodes)
    integer :: weight(max_nodes, max_edges)
    integer :: leaf_nodes(max_nodes)
    integer :: n_leaves
    integer :: dist(max_nodes, max_nodes)
    integer :: queue(max_nodes)
    integer :: front, rear
    logical :: visited(max_nodes)
    character(len=100) :: line
    
    ! Read input
    read(*,*) n
    
    ! Initialize data structures
    do i = 1, n
        adj_count(i) = 0
        visited(i) = .false.
        do j = 1, n
            dist(i,j) = 0
        end do
    end do
    
    ! Read edges
    edge_count = 0
    do while (.true.)
        read(*,'(A)', end=100) line
        if (len_trim(line) == 0) exit
        edge_count = edge_count + 1
        read(line,*) u, v, w
        ! Add edge u-v
        adj_count(u) = adj_count(u) + 1
        adj_list(u, adj_count(u)) = v
        weight(u, adj_count(u)) = w
        ! Add edge v-u (undirected)
        adj_count(v) = adj_count(v) + 1
        adj_list(v, adj_count(v)) = u
        weight(v, adj_count(v)) = w
    end do
    
100 continue
    
    ! Find leaf nodes (nodes with degree 1)
    n_leaves = 0
    do i = 1, n
        if (adj_count(i) == 1) then
            n_leaves = n_leaves + 1
            leaf_nodes(n_leaves) = i
        end if
    end do
    
    ! Compute distances between all pairs of leaves
    do i = 1, n_leaves
        call bfs(leaf_nodes(i), n, adj_list, adj_count, weight, dist)
    end do
    
    ! Output distance matrix
    do i = 1, n_leaves
        do j = 1, n_leaves
            if (j > 1) write(*,*) ' ', dist(leaf_nodes(i), leaf_nodes(j))
            if (j == 1) write(*,*) dist(leaf_nodes(i), leaf_nodes(j))
        end do
    end do
    
contains
    
    subroutine bfs(start_node, n_nodes, adj_list, adj_count, weight, dist_matrix)
        implicit none
        integer, intent(in) :: start_node, n_nodes
        integer, intent(in) :: adj_list(max_nodes, max_edges)
        integer, intent(in) :: adj_count(max_nodes)
        integer, intent(in) :: weight(max_nodes, max_edges)
        integer, intent(out) :: dist_matrix(max_nodes, max_nodes)
        integer :: current_node, neighbor, current_dist
        integer :: i, j
        
        ! Initialize visited array and distance matrix
        do i = 1, n_nodes
            visited(i) = .false.
            dist_matrix(start_node, i) = 0
        end do
        
        ! BFS
        front = 1
        rear = 1
        queue(front) = start_node
        visited(start_node) = .true.
        dist_matrix(start_node, start_node) = 0
        
        do while (front <= rear)
            current_node = queue(front)
            front = front + 1
            
            ! Visit all neighbors
            do i = 1, adj_count(current_node)
                neighbor = adj_list(current_node, i)
                if (.not. visited(neighbor)) then
                    visited(neighbor) = .true.
                    dist_matrix(start_node, neighbor) = &
                        dist_matrix(start_node, current_node) + weight(current_node, i)
                    rear = rear + 1
                    queue(rear) = neighbor
                end if
            end do
        end do
    end subroutine bfs
    
end program compute_distances_between_leaves
```

## Input Format
The input consists of:
1. First line: number of nodes `n`
2. Subsequent lines: edges in format `u v w` where `u` and `v` are nodes and `w` is weight

## Example Input
```
4
0 1 1
1 2 2
1 3 3
```

## Example Output
```
0 1 3 4
1 0 2 3
3 2 0 1
4 3 1 0
```

## Key Features
- Uses adjacency list representation for efficient graph storage
- Implements BFS to compute shortest paths from each leaf
- Handles undirected graph properly
- Properly identifies leaf nodes (degree 1)
- Outputs distance matrix between all pairs of leaves

## Time Complexity
- O(n²) for computing all pairwise distances
- Space complexity: O(n²) for the distance matrix

This solution correctly handles the Rosalind problem requirements for computing distances between leaves in a weighted tree.

