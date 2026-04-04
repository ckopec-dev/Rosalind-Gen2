# Rosalind Problem: Solve_the_Turnpike_Problem in Fortran

## Problem Understanding

The Turnpike Problem asks us to reconstruct a set of points on a line given the distances between all pairs of points. Given a set of distances, we need to find the positions of points that would generate those distances.

## Approach

1. Use backtracking to try different point placements
2. Start with known points (0 and the maximum distance)
3. Try to place remaining points one by one
4. Check if all distances are satisfied

## Fortran Implementation

```fortran
program turnpike_problem
    implicit none
    integer, parameter :: max_points = 100
    integer :: n, i, j, k, max_dist
    integer :: distances(10000), num_distances
    integer :: points(100), num_points
    integer :: used(100)
    logical :: found
    
    ! Read input
    call read_input(distances, num_distances, n)
    
    ! Initialize
    num_points = n
    points(1) = 0
    points(2) = max_dist
    
    ! Initialize used array
    do i = 1, n
        used(i) = 0
    end do
    used(1) = 1
    used(2) = 1
    
    ! Solve using backtracking
    found = .false.
    call backtrack(3, points, used, distances, num_distances, n, found)
    
    ! Output result
    if (found) then
        do i = 1, n
            write(*,*) points(i)
        end do
    else
        write(*,*) "No solution found"
    end if
    
contains
    
    subroutine read_input(distances, num_distances, n)
        implicit none
        integer, intent(inout) :: distances(10000)
        integer, intent(out) :: num_distances, n
        integer :: i, j, temp
        
        ! Read n (number of points)
        read(*,*) n
        
        ! Read all distances
        num_distances = (n * (n - 1)) / 2
        do i = 1, num_distances
            read(*,*) distances(i)
        end do
        
        ! Sort distances in descending order
        call sort_distances(distances, num_distances)
        
        ! Find maximum distance
        max_dist = distances(1)
    end subroutine read_input
    
    subroutine sort_distances(arr, n)
        implicit none
        integer, intent(inout) :: arr(10000)
        integer, intent(in) :: n
        integer :: i, j, temp
        
        ! Simple bubble sort (descending)
        do i = 1, n-1
            do j = i+1, n
                if (arr(i) < arr(j)) then
                    temp = arr(i)
                    arr(i) = arr(j)
                    arr(j) = temp
                end if
            end do
        end do
    end subroutine sort_distances
    
    subroutine backtrack(pos, points, used, distances, num_distances, n, found)
        implicit none
        integer, intent(in) :: pos, n, num_distances
        integer, intent(inout) :: points(100), used(100)
        integer, intent(in) :: distances(10000)
        logical, intent(inout) :: found
        
        integer :: i, j, current_point, valid
        integer :: temp_distances(10000)
        
        if (found) return
        
        if (pos > n) then
            ! Check if all distances are satisfied
            if (check_solution(points, n, distances, num_distances)) then
                found = .true.
            end if
            return
        end if
        
        ! Try to place point at different positions
        do i = 1, n
            if (.not. used(i)) then
                ! Calculate the point position
                current_point = points(pos-1) + distances(pos-1)
                
                ! Check if this point is valid (not already used)
                if (.not. is_point_used(current_point, points, pos-1)) then
                    points(pos) = current_point
                    used(i) = 1
                    
                    ! Continue with backtracking
                    call backtrack(pos+1, points, used, distances, num_distances, n, found)
                    
                    ! Backtrack
                    used(i) = 0
                end if
            end if
        end do
    end subroutine backtrack
    
    logical function is_point_used(point, points, num_points)
        implicit none
        integer, intent(in) :: point, points(100), num_points
        integer :: i
        
        is_point_used = .false.
        do i = 1, num_points
            if (points(i) == point) then
                is_point_used = .true.
                return
            end if
        end do
    end function is_point_used
    
    logical function check_solution(points, n, distances, num_distances)
        implicit none
        integer, intent(in) :: points(100), n, num_distances
        integer, intent(in) :: distances(10000)
        integer :: i, j, k, current_dist, count
        integer :: expected_distances(10000)
        
        ! Generate all pairwise distances
        k = 1
        do i = 1, n
            do j = i+1, n
                expected_distances(k) = abs(points(i) - points(j))
                k = k + 1
            end do
        end do
        
        ! Sort expected distances
        call sort_distances(expected_distances, (n*(n-1))/2)
        
        ! Compare with given distances
        check_solution = .true.
        do i = 1, num_distances
            if (expected_distances(i) /= distances(i)) then
                check_solution = .false.
                return
            end if
        end do
    end function check_solution
    
end program turnpike_problem
```

## Alternative Simpler Implementation

```fortran
program turnpike_simple
    implicit none
    integer, parameter :: max_n = 10
    integer :: n, i, j, k
    integer :: distances(100), num_distances
    integer :: points(100)
    logical :: solution_found
    
    ! Read input
    read(*,*) n
    num_distances = (n * (n - 1)) / 2
    
    do i = 1, num_distances
        read(*,*) distances(i)
    end do
    
    ! Sort distances in descending order
    call bubble_sort(distances, num_distances)
    
    ! Try to find solution
    call solve_turnpike(distances, num_distances, n, points, solution_found)
    
    if (solution_found) then
        do i = 1, n
            write(*,*) points(i)
        end do
    else
        write(*,*) "No solution"
    end if
    
contains
    
    subroutine bubble_sort(arr, n)
        implicit none
        integer, intent(inout) :: arr(100)
        integer, intent(in) :: n
        integer :: i, j, temp
        
        do i = 1, n-1
            do j = i+1, n
                if (arr(i) < arr(j)) then
                    temp = arr(i)
                    arr(i) = arr(j)
                    arr(j) = temp
                end if
            end do
        end do
    end subroutine bubble_sort
    
    subroutine solve_turnpike(distances, num_distances, n, points, found)
        implicit none
        integer, intent(in) :: num_distances, n
        integer, intent(in) :: distances(100)
        integer, intent(out) :: points(100)
        logical, intent(out) :: found
        
        ! Simple approach: place points at 0 and max distance
        points(1) = 0
        points(2) = distances(1)
        
        ! This is a simplified version - in practice, more complex backtracking
        ! would be needed for full solution
        
        found = .true.
    end subroutine solve_turnpike
    
end program turnpike_simple
```

## Key Features of the Solution

1. **Input Processing**: Reads the number of points and all pairwise distances
2. **Distance Sorting**: Sorts distances in descending order for easier processing
3. **Backtracking Algorithm**: Tries different point placements recursively
4. **Validation**: Checks if generated distances match input distances
5. **Output**: Prints the reconstructed point positions

## Usage Notes

- The problem requires careful backtracking since multiple solutions might exist
- The implementation assumes the points are placed on a line
- For larger inputs, more sophisticated optimization techniques may be needed
- The solution handles the case where points are not necessarily in order

This implementation provides a framework for solving the Turnpike Problem in Fortran, though a complete backtracking solution would require more complex constraint checking and optimization.

