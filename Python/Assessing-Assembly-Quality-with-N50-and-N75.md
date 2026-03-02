# Rosalind Problem: Assessing Assembly Quality with N50 and N75

## Problem Understanding

The N50 and N75 statistics are commonly used to assess the quality of genome assemblies. They represent the contig length at which 50% and 75% of the total assembly length is contained in contigs of that length or longer.

## Solution Approach

1. Read contig lengths from input
2. Sort contig lengths in descending order
3. Calculate cumulative sum of lengths
4. Find the N50 and N75 values

## Python Implementation

```python
def calculate_n50_n75(contig_lengths):
    """
    Calculate N50 and N75 statistics for a genome assembly.
    
    Args:
        contig_lengths: List of contig lengths
    
    Returns:
        tuple: (N50, N75) values
    """
    if not contig_lengths:
        return 0, 0
    
    # Sort contigs in descending order
    sorted_lengths = sorted(contig_lengths, reverse=True)
    
    # Calculate total assembly length
    total_length = sum(sorted_lengths)
    
    # Calculate cumulative sum
    cumulative_sum = 0
    n50 = 0
    n75 = 0
    
    for length in sorted_lengths:
        cumulative_sum += length
        
        # Find N50 (50% of total length)
        if n50 == 0 and cumulative_sum >= total_length * 0.5:
            n50 = length
            
        # Find N75 (75% of total length)
        if n75 == 0 and cumulative_sum >= total_length * 0.75:
            n75 = length
    
    return n50, n75

def main():
    # Read input contig lengths
    # In a real Rosalind problem, this would read from a file
    # For demonstration, using sample data
    contig_lengths = [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]
    
    n50, n75 = calculate_n50_n75(contig_lengths)
    
    print(f"N50: {n50}")
    print(f"N75: {n75}")

# Alternative implementation for Rosalind format
def rosalind_n50_n75(input_file):
    """
    Read contig lengths from file and calculate N50 and N75.
    """
    with open(input_file, 'r') as f:
        contig_lengths = [int(line.strip()) for line in f if line.strip()]
    
    n50, n75 = calculate_n50_n75(contig_lengths)
    
    return n50, n75

# Example usage with sample data
if __name__ == "__main__":
    # Sample input data
    sample_contigs = [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]
    
    n50, n75 = calculate_n50_n75(sample_contigs)
    
    print("Contig lengths:", sample_contigs)
    print(f"Total assembly length: {sum(sample_contigs)}")
    print(f"N50: {n50}")
    print(f"N75: {n75}")
    
    # Verification of calculation
    sorted_contigs = sorted(sample_contigs, reverse=True)
    print("\nSorted contigs:", sorted_contigs)
    
    cumulative = 0
    total = sum(sample_contigs)
    print(f"\nTotal length: {total}")
    print("Cumulative calculation:")
    
    for i, length in enumerate(sorted_contigs):
        cumulative += length
        percentage = cumulative / total * 100
        print(f"Contig {i+1}: {length}, Cumulative: {cumulative}, Percentage: {percentage:.1f}%")
```

## Step-by-Step Explanation

1. **Input Processing**: Read contig lengths (typically from a file)
2. **Sorting**: Sort contig lengths in descending order
3. **Cumulative Sum**: Calculate running total of contig lengths
4. **N50 Finding**: Find the smallest contig length where cumulative sum reaches 50% of total
5. **N75 Finding**: Find the smallest contig length where cumulative sum reaches 75% of total

## Example Walkthrough

For contig lengths: [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]

1. Sorted descending: [1000, 900, 800, 700, 600, 500, 400, 300, 200, 100]
2. Total length: 5500
3. 50% threshold: 2750
4. 75% threshold: 4125

Cumulative calculation:
- 1000: 1000 (18.2%)
- 1900: 1900 (34.5%)
- 2700: 2700 (49.1%) ← N50 = 800 (first contig that makes cumulative ≥ 2750)
- 3500: 3500 (63.6%)
- 4100: 4100 (74.5%) ← N75 = 600 (first contig that makes cumulative ≥ 4125)

## Final Answer Format

For Rosalind problems, the output should be formatted as:
```
N50_value N75_value
```

This implementation handles the core logic for calculating N50 and N75 statistics, which are essential metrics for evaluating genome assembly quality.

