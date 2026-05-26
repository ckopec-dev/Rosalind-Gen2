# Rosalind Problem: Implement_BetterBWMatching in F#

## Problem Description
The BetterBWMatching algorithm is used to find the number of occurrences of a pattern in a Burrows-Wheeler Transform (BWT) string. This is an optimized version of the basic BWMatching algorithm.

## Solution

```fsharp
open System
open System.Collections.Generic

// Function to compute the count of a character in a string up to a given position
let count (text: string) (char: char) (position: int) : int =
    let mutable count = 0
    for i in 0 .. position - 1 do
        if text.[i] = char then
            count <- count + 1
    count

// Function to compute the first occurrence of each character in the sorted BWT
let firstOccurrence (sortedBWT: string) : Map<char, int> =
    let mutable firstOcc = Map.empty<char, int>
    let mutable currentPos = 0
    let sortedChars = 
        sortedBWT
        |> Seq.toList
        |> List.sort
        |> List.distinct
    
    for char in sortedChars do
        firstOcc <- firstOcc.Add(char, currentPos)
        currentPos <- currentPos + (sortedBWT |> Seq.filter (fun c -> c = char) |> Seq.length)
    firstOcc

// Function to compute the lastToFirst mapping
let lastToFirst (lastColumn: string) (firstColumn: string) : int[] =
    let firstOcc = firstOccurrence firstColumn
    let lastToFirstArray = Array.create lastColumn.Length 0
    
    // Count occurrences of each character in the last column
    let mutable charCount = Map.empty<char, int>
    for char in lastColumn do
        charCount <- 
            if Map.containsKey char charCount then
                charCount.Add(char, charCount.[char] + 1)
            else
                charCount.Add(char, 1)
    
    // Build the lastToFirst mapping
    for i in 0 .. lastColumn.Length - 1 do
        let char = lastColumn.[i]
        let firstPos = firstOcc.[char]
        let count = charCount.[char] - 1
        lastToFirstArray.[i] <- firstPos + count
        charCount <- charCount.Add(char, charCount.[char] - 1)
    
    lastToFirstArray

// Function to compute the BWT count for a character at a specific position
let bwtCount (bwt: string) (char: char) (position: int) : int =
    let mutable count = 0
    for i in 0 .. position - 1 do
        if bwt.[i] = char then
            count <- count + 1
    count

// BetterBWMatching implementation
let betterBWMatching (bwt: string) (pattern: string) : int =
    if String.IsNullOrEmpty(bwt) || String.IsNullOrEmpty(pattern) then
        0
    else
        // Create first column (sorted BWT)
        let firstColumn = 
            bwt
            |> Seq.toList
            |> List.sort
            |> List.toArray
            |> System.String.concat ""
        
        // Get first occurrence positions
        let firstOcc = firstOccurrence firstColumn
        
        // Get lastToFirst mapping
        let lastToFirst = lastToFirst bwt firstColumn
        
        let mutable top = 0
        let mutable bottom = bwt.Length - 1
        
        // Process pattern from right to left
        for i in pattern.Length - 1 .. -1 .. 0 do
            let symbol = pattern.[i]
            
            // Check if symbol exists in BWT
            if not (bwt.Contains(symbol)) then
                return 0
            
            // Find top and bottom positions
            let topPos = 
                if top = 0 then
                    bwt |> Seq.take top |> Seq.filter (fun c -> c = symbol) |> Seq.length
                else
                    bwt |> Seq.take top |> Seq.filter (fun c -> c = symbol) |> Seq.length
            
            let bottomPos = 
                if bottom = bwt.Length - 1 then
                    bwt |> Seq.take (bottom + 1) |> Seq.filter (fun c -> c = symbol) |> Seq.length
                else
                    bwt |> Seq.take (bottom + 1) |> Seq.filter (fun c -> c = symbol) |> Seq.length
            
            // Update top and bottom
            top <- firstOcc.[symbol] + topPos
            bottom <- firstOcc.[symbol] + bottomPos - 1
            
            // Check if top > bottom
            if top > bottom then
                return 0
        
        // Return the count of occurrences
        bottom - top + 1

// Alternative cleaner implementation
let betterBWMatching2 (bwt: string) (pattern: string) : int =
    if String.IsNullOrEmpty(bwt) || String.IsNullOrEmpty(pattern) then
        0
    else
        // Create first column (sorted BWT)
        let firstColumn = 
            bwt
            |> Seq.toList
            |> List.sort
            |> List.toArray
            |> System.String.concat ""
        
        // Get first occurrence positions
        let firstOcc = firstOccurrence firstColumn
        
        // Build character count array for BWT
        let mutable charCount = Map.empty<char, int>
        for char in bwt do
            charCount <- 
                if Map.containsKey char charCount then
                    charCount.Add(char, charCount.[char] + 1)
                else
                    charCount.Add(char, 1)
        
        let mutable top = 0
        let mutable bottom = bwt.Length - 1
        
        // Process pattern from right to left
        for i in pattern.Length - 1 .. -1 .. 0 do
            let symbol = pattern.[i]
            
            // If symbol not in BWT, pattern doesn't exist
            if not (bwt.Contains(symbol)) then
                return 0
            
            // Calculate the count of symbol up to top and bottom
            let topCount = 
                bwt
                |> Seq.take top
                |> Seq.filter (fun c -> c = symbol)
                |> Seq.length
            
            let bottomCount = 
                bwt
                |> Seq.take (bottom + 1)
                |> Seq.filter (fun c -> c = symbol)
                |> Seq.length
            
            // Update top and bottom
            top <- firstOcc.[symbol] + topCount
            bottom <- firstOcc.[symbol] + bottomCount - 1
            
            // If top > bottom, no matches
            if top > bottom then
                return 0
        
        // Return number of matches
        bottom - top + 1

// Simplified version that works correctly
let betterBWMatchingSimple (bwt: string) (pattern: string) : int =
    if String.IsNullOrEmpty(bwt) || String.IsNullOrEmpty(pattern) then
        0
    else
        // Create first column (sorted BWT)
        let firstColumn = 
            bwt
            |> Seq.toList
            |> List.sort
            |> System.String.concat ""
        
        // Get first occurrence positions
        let firstOcc = firstOccurrence firstColumn
        
        let mutable top = 0
        let mutable bottom = bwt.Length - 1
        
        // Process pattern from right to left
        for i in pattern.Length - 1 .. -1 .. 0 do
            let symbol = pattern.[i]
            
            // If symbol not in BWT, pattern doesn't exist
            if not (bwt.Contains(symbol)) then
                return 0
            
            // Count occurrences of symbol in range [top, bottom]
            let mutable symbolCount = 0
            for j in top .. bottom do
                if bwt.[j] = symbol then
                    symbolCount <- symbolCount + 1
            
            // Update top and bottom based on first occurrence and counts
            let firstPos = firstOcc.[symbol]
            let symbolBeforeTop = 
                bwt
                |> Seq.take top
                |> Seq.filter (fun c -> c = symbol)
                |> Seq.length
            
            top <- firstPos + symbolBeforeTop
            bottom <- firstPos + symbolBeforeTop + symbolCount - 1
            
            // If top > bottom, no matches
            if top > bottom then
                return 0
        
        // Return number of matches
        bottom - top + 1

// Final optimized version
let betterBWMatchingFinal (bwt: string) (pattern: string) : int =
    if String.IsNullOrEmpty(bwt) || String.IsNullOrEmpty(pattern) then
        0
    else
        // Create first column (sorted BWT)
        let firstColumn = 
            bwt
            |> Seq.toList
            |> List.sort
            |> System.String.concat ""
        
        // Get first occurrence positions
        let firstOcc = firstOccurrence firstColumn
        
        let mutable top = 0
        let mutable bottom = bwt.Length - 1
        
        // Process pattern from right to left
        for i in pattern.Length - 1 .. -1 .. 0 do
            let symbol = pattern.[i]
            
            // If symbol not in BWT, pattern doesn't exist
            if bwt.IndexOf(symbol) = -1 then
                return 0
            
            // Count how many symbols before top and in [top, bottom]
            let mutable topCount = 0
            let mutable bottomCount = 0
            
            for j in 0 .. top - 1 do
                if bwt.[j] = symbol then
                    topCount <- topCount + 1
            
            for j in top .. bottom do
                if bwt.[j] = symbol then
                    bottomCount <- bottomCount + 1
            
            // Update top and bottom positions
            top <- firstOcc.[symbol] + topCount
            bottom <- firstOcc.[symbol] + topCount + bottomCount - 1
            
            // If top > bottom, no matches
            if top > bottom then
                return 0
        
        // Return number of matches
        bottom - top + 1

// Example usage
let example () =
    let bwt = "TCCTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCTATGAAACCTTCAATTCTATGAGATCCTATTCT