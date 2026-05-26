# Rosalind Problem: Counting Inversions in Rust

## Problem Understanding

An inversion in an array is a pair of indices (i, j) such that i < j but arr[i] > arr[j]. The task is to count the total number of such inversions in the given array.

## Approach

I'll use the merge sort algorithm to efficiently count inversions in O(n log n) time complexity. The key insight is that during the merge step, when we merge two sorted subarrays, we can count inversions by counting how many elements from the right subarray are smaller than elements from the left subarray.

## Solution

```rust
use std::io;

fn merge_and_count(arr: &mut Vec<i32>, temp: &mut Vec<i32>, left: usize, mid: usize, right: usize) -> i64 {
    let mut i = left;
    let mut j = mid + 1;
    let mut k = left;
    let mut inv_count = 0i64;
    
    // Merge the two subarrays while counting inversions
    while i <= mid && j <= right {
        if arr[i] <= arr[j] {
            temp[k] = arr[i];
            i += 1;
        } else {
            temp[k] = arr[j];
            // All elements from i to mid are greater than arr[j]
            // So we have (mid - i + 1) inversions
            inv_count += (mid - i + 1) as i64;
            j += 1;
        }
        k += 1;
    }
    
    // Copy remaining elements
    while i <= mid {
        temp[k] = arr[i];
        i += 1;
        k += 1;
    }
    
    while j <= right {
        temp[k] = arr[j];
        j += 1;
        k += 1;
    }
    
    // Copy back the merged elements
    for i in left..=right {
        arr[i] = temp[i];
    }
    
    inv_count
}

fn merge_sort_and_count(arr: &mut Vec<i32>, temp: &mut Vec<i32>, left: usize, right: usize) -> i64 {
    let mut inv_count = 0i64;
    
    if left < right {
        let mid = left + (right - left) / 2;
        
        inv_count += merge_sort_and_count(arr, temp, left, mid);
        inv_count += merge_sort_and_count(arr, temp, mid + 1, right);
        inv_count += merge_and_count(arr, temp, left, mid, right);
    }
    
    inv_count
}

fn count_inversions(arr: &mut Vec<i32>) -> i64 {
    let n = arr.len();
    if n <= 1 {
        return 0;
    }
    
    let mut temp = vec![0; n];
    merge_sort_and_count(arr, &mut temp, 0, n - 1)
}

fn main() {
    // Read input from stdin
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    
    // Parse the array
    let arr: Vec<i32> = input
        .trim()
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();
    
    // Count inversions
    let result = count_inversions(&mut arr.clone());
    
    println!("{}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_inversions() {
        let mut arr = vec![1, 3, 5, 2, 4, 6];
        let result = count_inversions(&mut arr);
        assert_eq!(result, 3); // Inversions: (3,2), (5,2), (5,4)
        
        let mut arr2 = vec![2, 3, 8, 6, 1];
        let result2 = count_inversions(&mut arr2);
        assert_eq!(result2, 5); // Inversions: (2,1), (3,1), (8,6), (8,1), (6,1)
        
        let mut arr3 = vec![1, 2, 3, 4, 5];
        let result3 = count_inversions(&mut arr3);
        assert_eq!(result3, 0); // No inversions
        
        let mut arr4 = vec![5, 4, 3, 2, 1];
        let result4 = count_inversions(&mut arr4);
        assert_eq!(result4, 10); // All pairs are inversions
    }
}
```

## Explanation

1. **Algorithm**: Uses merge sort approach to count inversions efficiently
2. **Time Complexity**: O(n log n) - same as merge sort
3. **Space Complexity**: O(n) for temporary array

## Key Steps:

1. **Divide**: Split array into two halves recursively
2. **Conquer**: Count inversions in left half, right half, and across the two halves
3. **Combine**: During merge, count inversions between left and right subarrays

## How Inversion Counting Works During Merge:

When merging two sorted subarrays, if an element from the right subarray is smaller than an element from the left subarray:
- All elements from the current position in the left subarray to the end are greater than this right element
- This gives us (mid - i + 1) inversions for that particular right element

## Sample Input/Output:

**Input**: `1 3 5 2 4 6`
**Output**: `3`

**Input**: `2 3 8 6 1`  
**Output**: `5`

The solution handles edge cases like empty arrays, single elements, and arrays with no inversions or all inversions.

