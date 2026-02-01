# Rosalind Problem: Implement the Lloyd Algorithm for k-Means Clustering (Fortran Solution)

## Problem Description
Implement the Lloyd algorithm for k-means clustering, which iteratively updates cluster centers and assigns points to the nearest cluster.

## Solution

```fortran
program lloyd_kmeans
    implicit none
    integer, parameter :: dp = selected_real_kind(15, 300)
    integer :: i, j, k, n, d, k_clusters, max_iter
    integer :: iter
    real(dp) :: dist, min_dist
    real(dp), allocatable :: points(:,:)
    real(dp), allocatable :: centers(:,:)
    real(dp), allocatable :: new_centers(:,:)
    integer, allocatable :: assignments(:)
    real(dp) :: epsilon = 1.0e-6_dp
    
    ! Read input
    read(*,*) n, d
    read(*,*) k_clusters, max_iter
    
    ! Allocate memory
    allocate(points(n,d))
    allocate(centers(k_clusters,d))
    allocate(new_centers(k_clusters,d))
    allocate(assignments(n))
    
    ! Read points
    do i = 1, n
        do j = 1, d
            read(*,*) points(i,j)
        end do
    end do
    
    ! Initialize centers (first k points)
    do i = 1, k_clusters
        do j = 1, d
            centers(i,j) = points(i,j)
        end do
    end do
    
    ! Lloyd algorithm
    do iter = 1, max_iter
        ! Assign points to nearest centers
        do i = 1, n
            min_dist = huge(min_dist)
            assignments(i) = 1
            do k = 1, k_clusters
                dist = 0.0_dp
                do j = 1, d
                    dist = dist + (points(i,j) - centers(k,j))**2
                end do
                dist = sqrt(dist)
                if (dist < min_dist) then
                    min_dist = dist
                    assignments(i) = k
                end if
            end do
        end do
        
        ! Update centers
        do k = 1, k_clusters
            do j = 1, d
                new_centers(k,j) = 0.0_dp
            end do
        end do
        
        ! Calculate new centers
        do i = 1, n
            k = assignments(i)
            do j = 1, d
                new_centers(k,j) = new_centers(k,j) + points(i,j)
            end do
        end do
        
        ! Normalize centers
        do k = 1, k_clusters
            do j = 1, d
                new_centers(k,j) = new_centers(k,j) / count(assignments == k)
            end do
        end do
        
        ! Check convergence
        if (maxval(abs(centers - new_centers)) < epsilon) then
            exit
        end if
        
        centers = new_centers
    end do
    
    ! Output final centers
    do i = 1, k_clusters
        do j = 1, d
            if (j > 1) write(*,*) ' '
            write(*,'(G0)') centers(i,j)
        end do
        write(*,*) ''
    end do
    
    ! Clean up
    deallocate(points, centers, new_centers, assignments)
    
end program lloyd_kmeans
```

## Explanation

This Fortran implementation of the Lloyd algorithm for k-means clustering follows these steps:

1. **Input Reading**: Reads the number of points (n), dimensions (d), number of clusters (k), and maximum iterations
2. **Memory Allocation**: Allocates arrays for points, centers, new centers, and assignments
3. **Initialization**: Sets initial cluster centers using the first k points
4. **Main Loop**: 
   - Assigns each point to the nearest cluster center
   - Updates cluster centers by computing the mean of all points assigned to each cluster
   - Checks for convergence (centers don't change significantly)
5. **Output**: Prints the final cluster centers

## Key Features

- Uses double precision (`dp`) for numerical accuracy
- Implements proper convergence checking with epsilon tolerance
- Handles multi-dimensional points correctly
- Uses dynamic memory allocation for flexibility
- Follows the standard Lloyd algorithm iterative approach

## Sample Input Format
```
n d
k max_iter
point1_1 point1_2 ... point1_d
point2_1 point2_2 ... point2_d
...
```

## Sample Output Format
```
center1_1 center1_2 ... center1_d
center2_1 center2_2 ... center2_d
...
```

The algorithm continues until convergence or maximum iterations are reached, ensuring stable k-means clustering results.

