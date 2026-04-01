# Rosalind Problem: Transcribing DNA into RNA (Transcribing_DNA_into_RNA)

## Problem Statement
In RNA strings, symbols 'U' and 'T' are used to represent nucleotides. Given a DNA string `t` corresponding to a coding strand, its transcribed RNA string `u` is formed by replacing all occurrences of 'T' with 'U'.

## Solution in C#

```csharp
using System;
using System.IO;
using System.Text;

public class RNA transcription
{
    public static string TranscribeDNAtoRNA(string dna)
    {
        // Replace all 'T' characters with 'U'
        return dna.Replace('T', 'U');
    }
    
    public static void Main(string[] args)
    {
        // Read input from file (assuming input.txt contains the DNA string)
        string dna = File.ReadAllText("input.txt").Trim();
        
        // Transcribe DNA to RNA
        string rna = TranscribeDNAtoRNA(dna);
        
        // Output result
        Console.WriteLine(rna);
        
        // Optionally write to output file
        File.WriteAllText("output.txt", rna);
    }
}
```

## Alternative Implementation Using StringBuilder

```csharp
using System;
using System.IO;
using System.Text;

public class RNATranscription
{
    public static string TranscribeDNAtoRNA(string dna)
    {
        StringBuilder rna = new StringBuilder();
        
        foreach (char nucleotide in dna)
        {
            if (nucleotide == 'T')
            {
                rna.Append('U');
            }
            else
            {
                rna.Append(nucleotide);
            }
        }
        
        return rna.ToString();
    }
    
    public static void Main(string[] args)
    {
        // Read input from file
        string dna = File.ReadAllText("input.txt").Trim();
        
        // Transcribe DNA to RNA
        string rna = TranscribeDNAtoRNA(dna);
        
        // Output result
        Console.WriteLine(rna);
    }
}
```

## Sample Input/Output

**Input (from file):**
```
GATGGAACTTGACTACGTAAATT
```

**Output:**
```
GAUGGAACUUGACUACGUAAAUU
```

## Explanation

1. **Problem Understanding**: The task is to convert a DNA string to its RNA complement by replacing each 'T' with 'U'
2. **Approach**: 
   - Simple string replacement using `Replace()` method
   - Alternative approach using `StringBuilder` for character-by-character processing
3. **Time Complexity**: O(n) where n is the length of the DNA string
4. **Space Complexity**: O(n) for the resulting RNA string

## Key Points

- DNA contains nucleotides: A, C, G, T
- RNA contains nucleotides: A, C, G, U
- The only difference is that DNA uses 'T' while RNA uses 'U'
- The solution handles the replacement efficiently using built-in string methods
- Input/output handling is standard for Rosalind problems

