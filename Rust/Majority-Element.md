# Rosalind Problem: Majority Element in Rust

## Problem Understanding

The majority element in an array is the element that appears more than ⌊n/2⌋ times, where n is the array length. We need to find such an element if it exists.

## Solution Approach

I'll use Boyer-Moore Majority Vote Algorithm which works in O(n) time and O(1) space:
1. Find a candidate element that could be the majority element
2. Verify if the candidate actually appears more than n/2 times

## Implementation

```rust
use std::io;

fn main() {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    
    let t: usize = input.trim().parse().unwrap();
    
    for _ in 0..t {
        input.clear();
        io::stdin().read_line(&mut input).unwrap();
        
        let n: usize = input.trim().parse().unwrap();
        
        input.clear();
        io::stdin().read_line(&mut input).unwrap();
        
        let numbers: Vec<i32> = input
            .trim()
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();
        
        let result = majority_element(&numbers);
        println!("{}", result);
    }
}

fn majority_element(numbers: &[i32]) -> i32 {
    let n = numbers.len();
    
    // Step 1: Find candidate using Boyer-Moore algorithm
    let mut candidate = 0;
    let mut count = 0;
    
    for &num in numbers {
        if count == 0 {
            candidate = num;
        }
        if num == candidate {
            count += 1;
        } else {
            count -= 1;
        }
    }
    
    // Step 2: Verify the candidate is actually majority element
    let mut candidate_count = 0;
    for &num in numbers {
        if num == candidate {
            candidate_count += 1;
        }
    }
    
    if candidate_count > n / 2 {
        candidate
    } else {
        -1 // No majority element
    }
}
```

## How It Works

1. **Boyer-Moore Voting Algorithm**: 
   - Initialize candidate and count variables
   - Iterate through array:
     - If count is 0, set current element as candidate
     - If current element equals candidate, increment count
     - Otherwise, decrement count
   - This finds a potential majority element

2. **Verification**:
   - Count actual occurrences of the candidate
   - If count > n/2, return the candidate
   - Otherwise, return -1 (no majority element)

## Example

Input:
```
2
4
1 2 3 4
5
1 2 3 4 5
```

Output:
```
-1
-1
```

Input:
```
2
5
3 3 4 2 4
5
1 1 1 1 1
```

Output:
```
-1
1
```

## Time and Space Complexity

- **Time Complexity**: O(n) - two passes through the array
- **Space Complexity**: O(1) - only using a constant amount of extra space

The algorithm is optimal for this problem as it requires examining each element at least once to determine the majority element.

