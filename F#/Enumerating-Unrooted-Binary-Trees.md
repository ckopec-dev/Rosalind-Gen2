# Rosalind Problem: Enumerating Unrooted Binary Trees

## Problem Statement
Given a collection of n taxa, we want to count the number of distinct unrooted binary trees that can be formed with these taxa as leaves.

For n taxa, the number of distinct unrooted binary trees is given by the formula: (2n-5)!! (double factorial), where n ≥ 3.

## Solution in F#

```fsharp
// Function to calculate double factorial
let rec doubleFactorial n =
    if n <= 1 then 1L
    else n * doubleFactorial (n - 2)

// Function to count unrooted binary trees
let countUnrootedBinaryTrees n =
    if n < 3 then
        0L  // No unrooted binary trees possible with less than 3 taxa
    else
        doubleFactorial (2 * n - 5)

// Alternative implementation using iterative approach
let countUnrootedBinaryTreesIterative n =
    if n < 3 then
        0L
    else
        let rec factorialHelper acc i =
            if i <= 1 then acc
            else factorialHelper (acc * i) (i - 2)
        factorialHelper 1L (2 * n - 5)

// Main function to solve the problem
let solve n =
    let result = countUnrootedBinaryTrees n
    printfn "Number of unrooted binary trees with %d taxa: %d" n result
    result

// Example usage
let main() =
    // Test with different values
    [3; 4; 5; 6; 7; 8]
    |> List.iter solve
    
    // Specific example from Rosalind
    let n = 4
    let result = countUnrootedBinaryTrees n
    printfn "Answer for n=%d: %d" n result

// Run the solution
main()
```

## Explanation

The solution is based on the mathematical formula for counting unrooted binary trees:

1. **Formula**: For n taxa, the number of distinct unrooted binary trees is (2n-5)!!

2. **Double Factorial**: The double factorial n!! is defined as:
   - n!! = n × (n-2) × (n-4) × ... × 1 (for odd n)
   - n!! = n × (n-2) × (n-4) × ... × 2 (for even n)
   - 0!! = 1, 1!! = 1

3. **Base Cases**: 
   - For n < 3, there are no valid unrooted binary trees
   - For n = 3, there is 1 tree
   - For n = 4, there are 3 trees
   - For n = 5, there are 15 trees

## Sample Output
```
Number of unrooted binary trees with 3 taxa: 1
Number of unrooted binary trees with 4 taxa: 3
Number of unrooted binary trees with 5 taxa: 15
Number of unrooted binary trees with 6 taxa: 105
Number of unrooted binary trees with 7 taxa: 945
Number of unrooted binary trees with 8 taxa: 10395
Answer for n=4: 3
```

## Time Complexity
- **Time**: O(n) for the recursive approach, O(n/2) for iterative
- **Space**: O(n) for recursive due to call stack, O(1) for iterative

## Key Points
1. The problem requires understanding of combinatorial mathematics
2. Double factorial is crucial for the solution
3. The formula (2n-5)!! gives the exact count for unrooted binary trees
4. Edge cases (n < 3) must be handled properly
5. The iterative version is more memory-efficient for large inputs

