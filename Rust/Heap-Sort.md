# Rosalind Problem: Heap Sort in Rust

## Problem Understanding

The Heap Sort problem requires implementing a heap sort algorithm to sort an array of integers in ascending order.

## Solution Approach

I'll implement heap sort using the following steps:
1. Build a max heap from the input array
2. Repeatedly extract the maximum element and rebuild the heap
3. Place extracted elements at the end of the array

## Rust Implementation

```rust
fn heap_sort(arr: &mut Vec<i32>) {
    let n = arr.len();
    
    // Build max heap
    for i in (0..n/2).rev() {
        heapify(arr, n, i);
    }
    
    // Extract elements from heap one by one
    for i in (1..n).rev() {
        // Move current root to end
        arr.swap(0, i);
        
        // Call heapify on the reduced heap
        heapify(arr, i, 0);
    }
}

fn heapify(arr: &mut Vec<i32>, n: usize, i: usize) {
    let mut largest = i;
    let left = 2 * i + 1;
    let right = 2 * i + 2;
    
    // If left child exists and is greater than root
    if left < n && arr[left] > arr[largest] {
        largest = left;
    }
    
    // If right child exists and is greater than largest so far
    if right < n && arr[right] > arr[largest] {
        largest = right;
    }
    
    // If largest is not root
    if largest != i {
        arr.swap(i, largest);
        heapify(arr, n, largest);
    }
}

fn main() {
    // Example usage with sample data
    let mut arr = vec![4, 10, 3, 5, 1];
    println!("Original array: {:?}", arr);
    
    heap_sort(&mut arr);
    println!("Sorted array: {:?}", arr);
    
    // For Rosalind input format
    // Read input from stdin
    use std::io;
    
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    
    // Parse the input
    let mut numbers: Vec<i32> = input
        .trim()
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();
    
    // Remove the first element (array size)
    numbers.remove(0);
    
    // Sort the array
    heap_sort(&mut numbers);
    
    // Print result
    println!("{}", numbers.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" "));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_heap_sort() {
        let mut arr = vec![4, 10, 3, 5, 1];
        heap_sort(&mut arr);
        assert_eq!(arr, vec![1, 3, 4, 5, 10]);
        
        let mut arr2 = vec![1, 2, 3, 4, 5];
        heap_sort(&mut arr2);
        assert_eq!(arr2, vec![1, 2, 3, 4, 5]);
        
        let mut arr3 = vec![5, 4, 3, 2, 1];
        heap_sort(&mut arr3);
        assert_eq!(arr3, vec![1, 2, 3, 4, 5]);
    }
}
```

## Explanation

### Key Components:

1. **`heap_sort` function**: Main sorting function that:
   - First builds a max heap using `heapify`
   - Then repeatedly extracts the maximum element and rebuilds the heap

2. **`heapify` function**: Maintains the heap property:
   - Compares parent with its children
   - Swaps if necessary to maintain max heap property
   - Recursively calls itself to fix any violations

3. **Algorithm Steps**:
   - Build max heap from input array
   - Extract maximum element (root) and place it at the end
   - Reduce heap size and restore heap property
   - Repeat until all elements are sorted

### Time Complexity: O(n log n)
### Space Complexity: O(1) (in-place sorting)

## Sample Input/Output

**Input:**
```
5
4 10 3 5 1
```

**Output:**
```
1 3 4 5 10
```

The implementation handles the Rosalind problem format where the first number indicates the array size, followed by the actual array elements to be sorted.

