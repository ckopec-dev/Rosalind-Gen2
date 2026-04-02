# Rosalind Problem: Assessing Assembly Quality with N50 and N75

## Problem Understanding

The N50 and N75 statistics are used to assess the quality of genome assemblies. They represent the contig length such that when contigs are ordered by length, the sum of contig lengths up to that point equals at least 50% and 75% of the total assembly length respectively.

## Solution in F#

```fsharp
open System
open System.IO

// Function to calculate N50 and N75 statistics
let calculateAssemblyStats (contigLengths: int64[]) =
    let totalLength = contigLengths |> Array.sum
    let sortedLengths = contigLengths |> Array.sortDescending
    
    // Calculate N50 (50% of total length)
    let n50Threshold = totalLength / 2L
    let mutable cumulativeLength = 0L
    let mutable n50 = 0L
    
    for length in sortedLengths do
        cumulativeLength <- cumulativeLength + length
        if cumulativeLength >= n50Threshold then
            n50 <- length
            break
    
    // Calculate N75 (75% of total length)
    let n75Threshold = totalLength * 3L / 4L
    let mutable cumulativeLength2 = 0L
    let mutable n75 = 0L
    
    for length in sortedLengths do
        cumulativeLength2 <- cumulativeLength2 + length
        if cumulativeLength2 >= n75Threshold then
            n75 <- length
            break
    
    (n50, n75)

// Main function to process input file
let processAssemblyFile (filename: string) =
    try
        // Read all lines from file
        let lines = File.ReadAllLines(filename)
        
        // Parse contig lengths (assuming they're in the first column)
        let contigLengths = 
            lines
            |> Array.filter (fun line -> not (String.IsNullOrWhiteSpace(line)))
            |> Array.map (fun line -> 
                let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                if parts.Length > 0 then
                    int64 parts.[0]
                else
                    0L)
            |> Array.filter (fun x -> x > 0L)
        
        // Calculate N50 and N75
        let (n50, n75) = calculateAssemblyStats contigLengths
        
        // Output results
        printfn "N50: %d" n50
        printfn "N75: %d" n75
        
        // Also output some additional statistics
        let totalLength = contigLengths |> Array.sum
        let contigCount = contigLengths.Length
        let avgLength = if contigCount > 0 then float totalLength / float contigCount else 0.0
        
        printfn "Total length: %d" totalLength
        printfn "Number of contigs: %d" contigCount
        printfn "Average length: %.2f" avgLength
        
        (n50, n75)
        
    with
    | ex -> 
        printfn "Error processing file: %s" ex.Message
        (0L, 0L)

// Alternative approach for direct input processing
let processContigList (contigLengths: int64[]) =
    let (n50, n75) = calculateAssemblyStats contigLengths
    
    printfn "N50: %d" n50
    printfn "N75: %d" n75
    
    (n50, n75)

// Example usage with sample data
let exampleUsage() =
    // Example contig lengths (as would be read from file)
    let sampleContigs = [|1000L; 500L; 2000L; 1500L; 300L; 800L; 1200L; 400L|]
    
    printfn "Sample contig lengths: %A" sampleContigs
    let (n50, n75) = processContigList sampleContigs
    
    printfn "Results:"
    printfn "N50: %d" n50
    printfn "N75: %d" n75

// Complete solution for Rosalind problem
let rosalindSolution (inputFile: string) =
    let result = processAssemblyFile inputFile
    result

// Run example
[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        // Process file from command line argument
        rosalindSolution argv.[0]
    else
        // Run example
        exampleUsage()
    
    0
```

## Explanation

### Key Functions:

1. **`calculateAssemblyStats`**: 
   - Takes an array of contig lengths
   - Calculates total assembly length
   - Sorts contigs in descending order
   - Finds N50 (length where cumulative sum reaches 50% of total)
   - Finds N75 (length where cumulative sum reaches 75% of total)

2. **`processAssemblyFile`**:
   - Reads input file line by line
   - Parses contig lengths (assumes first column contains lengths)
   - Handles empty lines and invalid data
   - Calls the statistics calculation function

3. **`processContigList`**:
   - Direct processing of contig lengths array
   - Useful for testing or when data is already parsed

### How N50/N75 Works:

1. Sort all contigs by length (descending)
2. Calculate cumulative sum of lengths
3. N50 is the length of the contig where cumulative sum reaches 50% of total
4. N75 is the length of the contig where cumulative sum reaches 75% of total

### Example:
For contigs with lengths [1000, 500, 2000, 1500, 300, 800, 1200, 400]:
- Total length = 7700
- Sorted: [2000, 1500, 1200, 1000, 800, 500, 400, 300]
- N50: First contig where cumulative sum ≥ 3850 (50% of 7700) = 1200
- N75: First contig where cumulative sum ≥ 5775 (75% of 7700) = 1500

### Usage:
```bash
# Compile and run
fsharpc AssemblyQuality.fs
./AssemblyQuality.exe input.txt
```

This solution handles the core requirements of the Rosalind problem and provides a robust implementation for calculating assembly quality metrics.

