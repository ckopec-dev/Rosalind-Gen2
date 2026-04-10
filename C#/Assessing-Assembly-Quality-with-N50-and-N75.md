# Rosalind Problem: Assessing Assembly Quality with N50 and N75

## Problem Understanding

The N50 and N75 statistics are used to assess the quality of genome assemblies. They represent the contig length at which 50% and 75% of the total assembly length is contained in contigs of that length or longer.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class AssemblyQuality
{
    public static void Main(string[] args)
    {
        // Read input from file
        string inputFile = "rosalind_assm.txt";
        string[] lines = File.ReadAllLines(inputFile);
        
        // Parse contig lengths
        List<long> contigLengths = new List<long>();
        
        foreach (string line in lines)
        {
            if (!string.IsNullOrWhiteSpace(line))
            {
                contigLengths.Add(long.Parse(line.Trim()));
            }
        }
        
        // Calculate N50 and N75
        var result = CalculateN50AndN75(contigLengths);
        
        Console.WriteLine($"{result.N50} {result.N75}");
    }
    
    public static (long N50, long N75) CalculateN50AndN75(List<long> contigLengths)
    {
        // Sort contigs in descending order
        var sortedContigs = contigLengths.OrderByDescending(x => x).ToList();
        
        // Calculate total assembly length
        long totalLength = sortedContigs.Sum();
        
        // Calculate cumulative lengths
        long cumulativeLength = 0;
        long n50 = 0;
        long n75 = 0;
        
        // Find N50 (length at which 50% of assembly is covered)
        foreach (long length in sortedContigs)
        {
            cumulativeLength += length;
            
            if (n50 == 0 && cumulativeLength >= totalLength / 2.0)
            {
                n50 = length;
            }
            
            if (n75 == 0 && cumulativeLength >= totalLength * 0.75)
            {
                n75 = length;
                break; // We found both values, can exit early
            }
        }
        
        // If we haven't found N75, it means the last contig is N75
        if (n75 == 0)
        {
            n75 = sortedContigs.Last();
        }
        
        return (n50, n75);
    }
}
```

## Alternative Implementation with More Detailed Approach

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class AssemblyQualityDetailed
{
    public static void Main(string[] args)
    {
        // Read input
        string inputFile = "rosalind_assm.txt";
        string[] lines = File.ReadAllLines(inputFile);
        
        // Parse contig lengths
        List<long> contigLengths = lines
            .Where(line => !string.IsNullOrWhiteSpace(line))
            .Select(line => long.Parse(line.Trim()))
            .ToList();
        
        // Calculate N50 and N75
        var (n50, n75) = CalculateN50AndN75(contigLengths);
        
        Console.WriteLine($"{n50} {n75}");
    }
    
    public static (long N50, long N75) CalculateN50AndN75(List<long> contigLengths)
    {
        if (contigLengths == null || contigLengths.Count == 0)
            throw new ArgumentException("Contig lengths list cannot be null or empty");
        
        // Sort in descending order
        var sortedContigs = contigLengths.OrderByDescending(x => x).ToList();
        
        // Calculate total assembly length
        long totalLength = sortedContigs.Sum();
        
        // Find N50 and N75
        long cumulativeLength = 0;
        long n50 = 0;
        long n75 = 0;
        
        foreach (long contigLength in sortedContigs)
        {
            cumulativeLength += contigLength;
            
            // N50: smallest contig length where cumulative length >= 50% of total
            if (n50 == 0 && cumulativeLength >= totalLength / 2.0)
            {
                n50 = contigLength;
            }
            
            // N75: smallest contig length where cumulative length >= 75% of total
            if (n75 == 0 && cumulativeLength >= totalLength * 0.75)
            {
                n75 = contigLength;
            }
            
            // Early exit if both values are found
            if (n50 != 0 && n75 != 0)
                break;
        }
        
        return (n50, n75);
    }
}
```

## How it works:

1. **Input parsing**: Read contig lengths from the input file
2. **Sorting**: Sort contigs in descending order of length
3. **Total calculation**: Calculate the total assembly length
4. **Cumulative sum**: Iterate through sorted contigs and calculate cumulative length
5. **N50 determination**: Find the smallest contig length where cumulative length ≥ 50% of total
6. **N75 determination**: Find the smallest contig length where cumulative length ≥ 75% of total

## Example:

For contig lengths [100, 200, 300, 400, 500]:
- Total length = 1500
- Sorted: [500, 400, 300, 200, 100]
- N50: smallest length where cumulative ≥ 750 → 500 (since 500 ≥ 750 is false, continue)
- Actually, cumulative = 500 (500), 900 (400+500), 1200 (300+400+500), etc.
- N50 = 400 (since 500+400 = 900 ≥ 750)
- N75 = 200 (since 500+400+300+200 = 1400 ≥ 1125)

## Time Complexity: O(n log n) due to sorting
## Space Complexity: O(n) for storing contig lengths

The solution correctly handles the Rosalind problem requirements and outputs the N50 and N75 values as space-separated integers.

