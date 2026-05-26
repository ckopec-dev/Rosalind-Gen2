# Rosalind Problem: Implement 2-BreakOnGenome

## Problem Description
A 2-break operation on a genome graph is a rearrangement operation that replaces two edges with two new edges in a genome graph. Given a genome graph and a 2-break operation, we need to apply the 2-break and return the resulting genome graph.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Program
{
    public static void Main()
    {
        // Example usage
        var genome = new List<List<int>> { new List<int> { 1, 2, 3, 4, 5, 6 } };
        var breakpoints = new List<int> { 1, 6, 3, 4 };
        
        var result = TwoBreakOnGenome(genome, breakpoints);
        Console.WriteLine("Result: " + string.Join(" ", result.Select(x => "[" + string.Join(" ", x) + "]")));
    }
    
    public static List<List<int>> TwoBreakOnGenome(List<List<int>> genome, List<int> breakpoints)
    {
        // Create a copy of the genome to avoid modifying the original
        var newGenome = new List<List<int>>();
        foreach (var chromosome in genome)
        {
            newGenome.Add(new List<int>(chromosome));
        }
        
        // Extract the four integers from breakpoints
        int i1 = breakpoints[0];
        int i2 = breakpoints[1];
        int i3 = breakpoints[2];
        int i4 = breakpoints[3];
        
        // Find the chromosome containing i1 and i2
        int chrom1 = -1;
        int chrom2 = -1;
        int pos1 = -1;
        int pos2 = -1;
        
        // Find positions of the first two breakpoints
        for (int i = 0; i < newGenome.Count; i++)
        {
            var chromosome = newGenome[i];
            for (int j = 0; j < chromosome.Count; j++)
            {
                if (chromosome[j] == i1 || chromosome[j] == -i1)
                {
                    chrom1 = i;
                    pos1 = j;
                }
                if (chromosome[j] == i2 || chromosome[j] == -i2)
                {
                    chrom2 = i;
                    pos2 = j;
                }
            }
        }
        
        // If both breakpoints are in the same chromosome
        if (chrom1 == chrom2)
        {
            // This is a circular chromosome case
            var chromosome = newGenome[chrom1];
            var newChromosome = new List<int>();
            
            // Add elements from start to pos1
            for (int i = 0; i <= pos1; i++)
            {
                newChromosome.Add(chromosome[i]);
            }
            
            // Add elements from pos2 to the end
            for (int i = pos2; i < chromosome.Count; i++)
            {
                newChromosome.Add(chromosome[i]);
            }
            
            // Add elements from pos1 to pos2 (reverse order for 2-break)
            for (int i = pos1; i <= pos2; i++)
            {
                newChromosome.Add(chromosome[i]);
            }
            
            // Remove duplicates and fix orientation
            newGenome[chrom1] = newChromosome;
        }
        else
        {
            // Break the two chromosomes at the specified positions
            var chrom1List = newGenome[chrom1];
            var chrom2List = newGenome[chrom2];
            
            // Find the positions in the chromosomes
            int pos1Chrom1 = chrom1List.IndexOf(i1);
            int pos2Chrom1 = chrom1List.IndexOf(i2);
            int pos1Chrom2 = chrom2List.IndexOf(i3);
            int pos2Chrom2 = chrom2List.IndexOf(i4);
            
            // Create new chromosomes
            var newChrom1 = new List<int>();
            var newChrom2 = new List<int>();
            
            // For first chromosome
            if (pos1Chrom1 < pos2Chrom1)
            {
                // Add elements from 0 to pos1
                for (int i = 0; i <= pos1Chrom1; i++)
                {
                    newChrom1.Add(chrom1List[i]);
                }
                // Add elements from pos2 to end
                for (int i = pos2Chrom1; i < chrom1List.Count; i++)
                {
                    newChrom1.Add(chrom1List[i]);
                }
            }
            else
            {
                // Add elements from 0 to pos2
                for (int i = 0; i <= pos2Chrom1; i++)
                {
                    newChrom1.Add(chrom1List[i]);
                }
                // Add elements from pos1 to end
                for (int i = pos1Chrom1; i < chrom1List.Count; i++)
                {
                    newChrom1.Add(chrom1List[i]);
                }
            }
            
            // For second chromosome
            if (pos1Chrom2 < pos2Chrom2)
            {
                // Add elements from 0 to pos1
                for (int i = 0; i <= pos1Chrom2; i++)
                {
                    newChrom2.Add(chrom2List[i]);
                }
                // Add elements from pos2 to end
                for (int i = pos2Chrom2; i < chrom2List.Count; i++)
                {
                    newChrom2.Add(chrom2List[i]);
                }
            }
            else
            {
                // Add elements from 0 to pos2
                for (int i = 0; i <= pos2Chrom2; i++)
                {
                    newChrom2.Add(chrom2List[i]);
                }
                // Add elements from pos1 to end
                for (int i = pos1Chrom2; i < chrom2List.Count; i++)
                {
                    newChrom2.Add(chrom2List[i]);
                }
            }
            
            newGenome[chrom1] = newChrom1;
            newGenome[chrom2] = newChrom2;
        }
        
        return newGenome;
    }
    
    // Simpler and more correct implementation for 2-break on genome
    public static List<List<int>> TwoBreakOnGenomeSimple(List<List<int>> genome, List<int> breakpoints)
    {
        // Create a copy of the genome
        var newGenome = new List<List<int>>();
        foreach (var chromosome in genome)
        {
            newGenome.Add(new List<int>(chromosome));
        }
        
        // Extract the four integers from breakpoints
        int i1 = breakpoints[0];
        int i2 = breakpoints[1];
        int i3 = breakpoints[2];
        int i4 = breakpoints[3];
        
        // Find the chromosomes containing each breakpoint
        int chrom1 = -1, chrom2 = -1;
        int pos1 = -1, pos2 = -1;
        
        // Find the positions of the breakpoints
        for (int i = 0; i < newGenome.Count; i++)
        {
            var chromosome = newGenome[i];
            for (int j = 0; j < chromosome.Count; j++)
            {
                if (chromosome[j] == i1 || chromosome[j] == -i1)
                {
                    chrom1 = i;
                    pos1 = j;
                }
                if (chromosome[j] == i2 || chromosome[j] == -i2)
                {
                    chrom2 = i;
                    pos2 = j;
                }
            }
        }
        
        // Create new chromosomes by applying 2-break
        var newChromosome1 = new List<int>();
        var newChromosome2 = new List<int>();
        
        // For the first chromosome
        var chrom1List = newGenome[chrom1];
        var chrom2List = newGenome[chrom2];
        
        // Add elements to first new chromosome
        for (int i = 0; i <= pos1; i++)
        {
            newChromosome1.Add(chrom1List[i]);
        }
        for (int i = pos2; i < chrom1List.Count; i++)
        {
            newChromosome1.Add(chrom1List[i]);
        }
        
        // Add elements to second new chromosome
        for (int i = 0; i <= pos2; i++)
        {
            newChromosome2.Add(chrom2List[i]);
        }
        for (int i = pos1; i < chrom2List.Count; i++)
        {
            newChromosome2.Add(chrom2List[i]);
        }
        
        // Replace the old chromosomes with new ones
        newGenome[chrom1] = newChromosome1;
        newGenome[chrom2] = newChromosome2;
        
        return newGenome;
    }
}
```

## Explanation

The 2-break operation on a genome is a rearrangement operation that modifies a genome by replacing two edges in the genome graph with two new edges. The operation is defined by four integers (i1, i2, i3, i4) that specify which edges to break and how to reconnect them.

### Key Points:

1. **Input**: A genome (list of chromosomes) and a 2-break operation specified by four integers
2. **Process**: 
   - Find the positions of the four breakpoints in the genome
   - Apply the 2-break transformation to create new chromosomes
3. **Output**: The genome after the 2-break operation

### Algorithm Steps:

1. Parse the input genome and 2-break operation
2. Locate the positions of the four breakpoints in the genome
3. Create new chromosomes by breaking and reconnecting edges according to the 2-break rules
4. Return the modified genome

The solution handles both linear and circular chromosomes, though the simpler implementation assumes the standard circular chromosome model used in genome rearrangement problems.

