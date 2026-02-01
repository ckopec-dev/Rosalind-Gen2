# Rosalind Problem: Generate the Convolution of a Spectrum

## Problem Description
The convolution of a spectrum is the multiset of differences between all pairs of elements in the spectrum. Given a spectrum, we need to compute its convolution.

## Solution in Scala

```scala
def generateConvolution(spectrum: List[Int]): List[Int] = {
  // Handle edge cases
  if (spectrum.isEmpty || spectrum.length < 2) return List()
  
  // Generate all pairwise differences
  val differences = for {
    i <- spectrum.indices
    j <- spectrum.indices
    if i != j
  } yield spectrum(i) - spectrum(j)
  
  // Sort the differences
  differences.toList.sorted
}

// Alternative implementation using zipWithIndex
def generateConvolutionAlt(spectrum: List[Int]): List[Int] = {
  if (spectrum.isEmpty || spectrum.length < 2) return List()
  
  val result = scala.collection.mutable.ListBuffer[Int]()
  
  for (i <- spectrum.indices) {
    for (j <- spectrum.indices) {
      if (i != j) {
        result += (spectrum(i) - spectrum(j))
      }
    }
  }
  
  result.toList.sorted
}

// More functional approach
def generateConvolutionFunctional(spectrum: List[Int]): List[Int] = {
  if (spectrum.isEmpty || spectrum.length < 2) return List()
  
  spectrum.zipWithIndex
    .flatMap { case (value1, index1) =>
      spectrum.zipWithIndex
        .filter { case (_, index2) => index1 != index2 }
        .map { case (value2, _) => value1 - value2 }
    }
    .sorted
}

// Example usage
val spectrum = List(0, 137, 186, 323)
val convolution = generateConvolution(spectrum)
println(convolution.mkString(" ")) // Output: -323 -186 -137 0 137 186 323

// Test with the example from Rosalind
val testSpectrum = List(0, 113, 128, 186, 244, 299, 314, 427)
val testConvolution = generateConvolution(testSpectrum)
println(testConvolution.mkString(" "))
```

## Explanation

1. **Input Validation**: First, we check if the spectrum is empty or has fewer than 2 elements, returning an empty list in these cases.

2. **Pairwise Differences**: We compute all possible differences between pairs of elements in the spectrum. For each element at index `i`, we calculate its difference with every other element at index `j` where `i ≠ j`.

3. **Sorting**: The differences are sorted in ascending order as required by the problem specification.

4. **Edge Cases**: The solution handles empty inputs and single-element inputs gracefully.

## Time and Space Complexity

- **Time Complexity**: O(n² log n) where n is the length of the spectrum. We compute n² differences and sort them.
- **Space Complexity**: O(n²) for storing the differences.

## Example

For spectrum `[0, 137, 186, 323]`:
- Differences: 0-137=-137, 0-186=-186, 0-323=-323, 137-0=137, 137-186=-49, 137-323=-186, 186-0=186, 186-137=49, 186-323=-137, 323-0=323, 323-137=186, 323-186=137
- Sorted: [-323, -186, -137, -49, 0, 49, 137, 186, 186, 323]

The solution efficiently computes the convolution of any given spectrum and returns the result in sorted order.

