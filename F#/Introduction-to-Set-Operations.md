# Rosalind Problem: Introduction to Set Operations - F# Solution

## Problem Understanding

This problem requires implementing basic set operations (union, intersection, difference) on two sets of integers, with the sets being subsets of a universal set U = {1, 2, ..., n}.

## Solution

```fsharp
open System
open System.IO

// Read input from file
let readInput filename =
    let lines = File.ReadAllLines(filename)
    let n = Int32.Parse(lines.[0])
    let set1 = lines.[1] |> fun s -> s.TrimStart('{').TrimEnd('}').Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> Int32.Parse(x.Trim()))
    let set2 = lines.[2] |> fun s -> s.TrimStart('{').TrimEnd('}').Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> Int32.Parse(x.Trim()))
    (n, set1, set2)

// Convert array to set
let toSet arr = Set.ofArray arr

// Perform set operations
let performSetOperations n set1 set2 =
    let u = Set.ofList [1..n]  // Universal set
    let s1 = toSet set1
    let s2 = toSet set2
    
    // Union: s1 ∪ s2
    let union = Set.union s1 s2
    
    // Intersection: s1 ∩ s2
    let intersection = Set.intersect s1 s2
    
    // Difference: s1 - s2
    let difference1 = Set.difference s1 s2
    
    // Difference: s2 - s1
    let difference2 = Set.difference s2 s1
    
    // Complement of s1: U - s1
    let complement1 = Set.difference u s1
    
    // Complement of s2: U - s2
    let complement2 = Set.difference u s2
    
    (union, intersection, difference1, difference2, complement1, complement2)

// Format set for output (as required by Rosalind)
let formatSet (s: Set<int>) =
    if Set.isEmpty s then
        "{}"
    else
        s |> Set.toList 
          |> List.sort 
          |> String.concat ", "
          |> fun x -> "{" + x + "}"

// Main execution
let main argv =
    if argv.Length < 1 then
        printfn "Usage: Rosalind_SetOperations.exe <input_file>"
        exit 1
    
    let inputFile = argv.[0]
    let (n, set1, set2) = readInput inputFile
    let (union, intersection, diff1, diff2, comp1, comp2) = performSetOperations n set1 set2
    
    // Output results
    printfn "%s" (formatSet union)
    printfn "%s" (formatSet intersection)
    printfn "%s" (formatSet diff1)
    printfn "%s" (formatSet diff2)
    printfn "%s" (formatSet comp1)
    printfn "%s" (formatSet comp2)

// Run main function if called directly
[<EntryPoint>]
let entryPoint argv =
    main argv
    0
```

## Example Usage

For input file `input.txt`:
```
10
{1, 2, 3, 4, 5}
{2, 4, 6, 8, 10}
```

The output would be:
```
{1, 2, 3, 4, 5, 6, 8, 10}
{2, 4}
{1, 3, 5}
{6, 8, 10}
{6, 7, 8, 9, 10}
{1, 3, 5, 7, 9}
```

## Explanation

1. **Input Parsing**: Read the universal set size `n` and two sets from the input file
2. **Set Conversion**: Convert arrays to F# sets for efficient operations
3. **Set Operations**:
   - Union: All elements in either set
   - Intersection: Elements in both sets
   - Difference: Elements in first set but not second
   - Complement: Elements in universal set but not in given set
4. **Output Formatting**: Format sets as required by Rosalind (sorted, comma-separated, enclosed in braces)

## Key F# Features Used

- **Set operations**: `Set.union`, `Set.intersect`, `Set.difference`
- **Functional programming**: Immutable data structures, pure functions
- **Pattern matching**: For handling different data types
- **List operations**: Sorting and string conversion
- **File I/O**: Reading input from files

This solution efficiently handles the set operations using F#'s built-in Set module and properly formats the output according to Rosalind's requirements.

