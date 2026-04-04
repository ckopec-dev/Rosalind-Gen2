# Rosalind Problem: Implement the Soft k-Means Clustering Algorithm in Fortran

## Problem Understanding

The soft k-means clustering algorithm is a variant of k-means where each data point belongs to each cluster with a probability (rather than being assigned to exactly one cluster). The algorithm iteratively updates cluster centers and membership probabilities.

## Solution Approach

1. **Initialize cluster centers randomly**
2. **Compute membership probabilities** using the soft assignment formula
3. **Update cluster centers** using weighted averages
4. **Repeat** until convergence or maximum iterations

## Fortran Implementation

```fortran
program soft_kmeans
    implicit none
    integer, parameter :: dp = selected_real_kind(15, 300)
    integer, parameter :: max_iter = 100
    integer, parameter :: eps = 1e-6
    
    integer :: n, d, k, i, j, iter
    real(dp) :: lambda
    real(dp), allocatable :: data(:,:)
    real(dp), allocatable :: centers(:,:)
    real(dp), allocatable :: membership(:,:)
    real(dp), allocatable :: new_centers(:,:)
    real(dp) :: diff, threshold
    
    ! Read input
    read(*,*) n, d, k, lambda
    
    ! Allocate memory
    allocate(data(n,d))
    allocate(centers(k,d))
    allocate(membership(n,k))
    allocate(new_centers(k,d))
    
    ! Read data points
    do i = 1, n
        read(*,*) (data(i,j), j=1,d)
    end do
    
    ! Initialize centers randomly
    call random_seed()
    do i = 1, k
        do j = 1, d
            call random_number(centers(i,j))
            centers(i,j) = centers(i,j) * 10.0_dp  ! Scale to reasonable range
        end do
    end do
    
    ! Main iteration loop
    do iter = 1, max_iter
        ! Compute membership probabilities
        do i = 1, n
            do j = 1, k
                membership(i,j) = compute_membership(data(i,:), centers(j,:), lambda)
            end do
        end do
        
        ! Normalize membership probabilities
        do i = 1, n
            call normalize_membership(membership(i,:))
        end do
        
        ! Update centers
        do j = 1, k
            do i = 1, d
                new_centers(j,i) = 0.0_dp
                do l = 1, n
                    new_centers(j,i) = new_centers(j,i) + membership(l,j) * data(l,i)
                end do
                new_centers(j,i) = new_centers(j,i) / sum(membership(:,j))
            end do
        end do
        
        ! Check for convergence
        diff = 0.0_dp
        do i = 1, k
            do j = 1, d
                diff = diff + (centers(i,j) - new_centers(i,j))**2
            end do
        end do
        
        if (sqrt(diff) < eps) then
            exit
        end if
        
        centers = new_centers
    end do
    
    ! Output results
    do i = 1, k
        do j = 1, d
            write(*,*) centers(i,j)
        end do
    end do
    
    deallocate(data, centers, membership, new_centers)
    
contains
    
    ! Compute membership probability using soft assignment
    real(dp) function compute_membership(point, center, lambda)
        real(dp), intent(in) :: point(:), center(:)
        real(dp), intent(in) :: lambda
        integer :: i
        real(dp) :: dist_sq, sum_exp
        
        dist_sq = 0.0_dp
        do i = 1, size(point)
            dist_sq = dist_sq + (point(i) - center(i))**2
        end do
        
        compute_membership = exp(-lambda * dist_sq)
    end function compute_membership
    
    ! Normalize membership probabilities to sum to 1
    subroutine normalize_membership(membership_vec)
        real(dp), intent(inout) :: membership_vec(:)
        real(dp) :: sum_mem
        integer :: i
        
        sum_mem = sum(membership_vec)
        if (sum_mem > 0.0_dp) then
            do i = 1, size(membership_vec)
                membership_vec(i) = membership_vec(i) / sum_mem
            end do
        end if
    end subroutine normalize_membership
    
end program soft_kmeans
```

## Input Format

The input should be provided in the following format:
```
n d k lambda
x11 x12 ... x1d
x21 x22 ... x2d
...
xn1 xn2 ... xnd
```

Where:
- `n`: number of data points
- `d`: dimensionality of data points
- `k`: number of clusters
- `lambda`: soft parameter (controls the softness of assignment)
- Each `xi` represents a data point in d-dimensional space

## Example Usage

For input:
```
4 2 2 1.0
1.0 2.0
3.0 4.0
5.0 6.0
7.0 8.0
```

The program will output the final cluster centers.

## Key Features

1. **Soft Assignment**: Uses exponential decay with parameter `lambda` to compute membership probabilities
2. **Convergence Check**: Stops when cluster centers change by less than epsilon
3. **Memory Management**: Proper allocation and deallocation of arrays
4. **Numerical Stability**: Includes normalization of membership probabilities

## Time Complexity

- **Time**: O(nkd × iterations) where iterations is the number of convergence steps
- **Space**: O(nk + kd) for membership matrix and cluster centers

This implementation provides a robust solution to the soft k-means clustering problem in Fortran, following the standard algorithmic approach while maintaining numerical precision through the use of double precision arithmetic.

