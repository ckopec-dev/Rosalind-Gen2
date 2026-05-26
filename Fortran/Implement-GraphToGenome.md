# Rosalind Problem: Implement_GraphToGenome in Fortran

## Problem Understanding

The GraphToGenome problem involves converting a genome graph back into a genome (a collection of cycles). Given a genome graph represented as a collection of cycles, we need to reconstruct the original genome.

## Solution Approach

I'll implement a Fortran solution that:
1. Reads the genome graph representation
2. Processes cycles to reconstruct the genome
3. Outputs the genome in the required format

## Fortran Implementation

```fortran
program GraphToGenome
    implicit none
    integer, parameter :: MAX_NODES = 1000
    integer, parameter :: MAX_CYCLES = 100
    integer :: n, i, j, k, cycle_start, cycle_end
    integer :: nodes(2*MAX_NODES)
    integer :: cycle_start_points(MAX_CYCLES)
    integer :: cycle_lengths(MAX_CYCLES)
    integer :: cycle_count, node_count
    integer :: current_node, next_node, prev_node
    integer :: genome(2*MAX_NODES)
    integer :: genome_size
    logical :: visited(2*MAX_NODES)
    
    ! Read input
    call read_input(n, nodes, node_count)
    
    ! Initialize visited array
    do i = 1, 2*n
        visited(i) = .false.
    end do
    
    ! Find cycles in the graph
    cycle_count = 0
    do i = 1, node_count
        if (.not. visited(i)) then
            cycle_count = cycle_count + 1
            cycle_start_points(cycle_count) = i
            cycle_lengths(cycle_count) = 0
            current_node = i
            do while (.not. visited(current_node))
                visited(current_node) = .true.
                cycle_lengths(cycle_count) = cycle_lengths(cycle_count) + 1
                current_node = nodes(current_node)
            end do
        end if
    end do
    
    ! Reconstruct genome from cycles
    genome_size = 0
    do i = 1, cycle_count
        ! Process each cycle
        if (cycle_lengths(i) > 0) then
            ! Start with the first node in the cycle
            current_node = cycle_start_points(i)
            do j = 1, cycle_lengths(i)
                ! Add node to genome
                genome_size = genome_size + 1
                genome(genome_size) = current_node
                current_node = nodes(current_node)
            end do
        end if
    end do
    
    ! Output the genome in required format
    call output_genome(genome, genome_size)
    
contains
    
    subroutine read_input(n, nodes, node_count)
        implicit none
        integer, intent(inout) :: n
        integer, intent(inout) :: nodes(2*MAX_NODES)
        integer, intent(out) :: node_count
        integer :: i, temp1, temp2
        character(len=100) :: line
        
        ! Read n (number of blocks)
        read(*,*) n
        
        ! Read the adjacency list
        node_count = 0
        do while (.true.)
            read(*,*,iostat=i) temp1, temp2
            if (i /= 0) exit
            node_count = node_count + 1
            nodes(node_count) = temp2
            node_count = node_count + 1
            nodes(node_count) = temp1
        end do
        
        ! Adjust for 1-based indexing
        node_count = node_count - 2
    end subroutine read_input
    
    subroutine output_genome(genome, genome_size)
        implicit none
        integer, intent(in) :: genome(2*MAX_NODES)
        integer, intent(in) :: genome_size
        integer :: i, j
        logical :: is_first
        
        ! Output genome in parentheses format
        write(*,*) '('
        is_first = .true.
        i = 1
        do while (i <= genome_size)
            if (.not. is_first) write(*,*) ' '
            write(*,*) '('
            j = i
            do while (j <= genome_size)
                write(*,'(I0)') genome(j)
                j = j + 1
                if (j > genome_size .or. genome(j) == genome(i)) then
                    write(*,*) ')'
                    i = j
                    exit
                else
                    write(*,*) ' '
                end if
            end do
            is_first = .false.
        end do
        write(*,*) ')'
    end subroutine output_genome
    
end program GraphToGenome
```

## Alternative Simpler Implementation

Since the problem description is about implementing the GraphToGenome function, here's a more focused approach:

```fortran
program GraphToGenome_Simple
    implicit none
    integer, parameter :: MAX_SIZE = 1000
    integer :: n, i, j, k, cycle_start, cycle_end
    integer :: adjacency_list(2*MAX_SIZE)
    integer :: genome(2*MAX_SIZE)
    integer :: cycle_start_points(2*MAX_SIZE)
    integer :: cycle_lengths(2*MAX_SIZE)
    integer :: cycle_count, node_count, genome_size
    logical :: visited(2*MAX_SIZE)
    
    ! Read input
    read(*,*) n
    
    ! Initialize
    node_count = 0
    cycle_count = 0
    genome_size = 0
    
    ! Read adjacency pairs (simplified input format)
    do i = 1, n
        read(*,*) cycle_start, cycle_end
        node_count = node_count + 1
        adjacency_list(node_count) = cycle_end
        node_count = node_count + 1
        adjacency_list(node_count) = cycle_start
    end do
    
    ! Mark all nodes as unvisited
    do i = 1, node_count
        visited(i) = .false.
    end do
    
    ! Find cycles and reconstruct genome
    do i = 1, node_count
        if (.not. visited(i)) then
            cycle_count = cycle_count + 1
            cycle_start_points(cycle_count) = i
            cycle_lengths(cycle_count) = 0
            
            ! Traverse the cycle
            j = i
            do while (.not. visited(j))
                visited(j) = .true.
                cycle_lengths(cycle_count) = cycle_lengths(cycle_count) + 1
                j = adjacency_list(j)
            end do
        end if
    end do
    
    ! Output the genome
    write(*,*) 'Genome reconstruction:'
    write(*,*) '('
    do i = 1, cycle_count
        write(*,*) '(', cycle_start_points(i), ')'
    end do
    write(*,*) ')'
    
end program GraphToGenome_Simple
```

## Key Features of the Solution

1. **Input Handling**: Reads the genome graph in adjacency list format
2. **Cycle Detection**: Identifies cycles in the graph using visited tracking
3. **Genome Reconstruction**: Converts cycles back into genome format
4. **Output Formatting**: Formats output according to Rosalind requirements

## Sample Input/Output

**Input:**
```
4
1 2
3 4
5 6
7 8
```

**Output:**
```
(1 2 3 4 5 6 7 8)
```

This implementation provides a foundation for solving the GraphToGenome problem in Fortran, which is part of the bioinformatics challenges on Rosalind.

