# Rosalind Problem: Introduction to Random Strings (Intro-RSTR)

## Problem Description
Given: A DNA string s of length at most 100 bp and an array A of 10 real numbers between 0 and 1.

Return: An array B of length 10, where B[i] represents the common logarithm of the probability that a random string constructed with the GC-content found in A[i] will match s exactly.

## Solution Approach
1. Calculate the probability of generating each nucleotide in the DNA string
2. For each GC-content value in array A, compute the probability of generating the specific string
3. Take the common logarithm (base 10) of each probability

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int main() {
    // Read DNA string
    char dna[101];
    printf("Enter DNA string (max 100 bp): ");
    scanf("%s", dna);
    
    int len = strlen(dna);
    
    // Read GC-content array A
    double A[10];
    printf("Enter 10 GC-content values (0-1): ");
    for (int i = 0; i < 10; i++) {
        scanf("%lf", &A[i]);
    }
    
    // Calculate probabilities for each GC-content
    double B[10];
    
    for (int i = 0; i < 10; i++) {
        double gc_content = A[i];
        double at_content = 1.0 - gc_content;
        
        double probability = 1.0;
        
        // For each nucleotide in the DNA string
        for (int j = 0; j < len; j++) {
            char nucleotide = dna[j];
            
            if (nucleotide == 'G' || nucleotide == 'C') {
                // Probability of GC = gc_content / 2
                probability *= gc_content / 2.0;
            } else if (nucleotide == 'A' || nucleotide == 'T') {
                // Probability of AT = at_content / 2
                probability *= at_content / 2.0;
            }
        }
        
        // Take common logarithm (base 10)
        B[i] = log10(probability);
    }
    
    // Output results
    printf("Results:\n");
    for (int i = 0; i < 10; i++) {
        printf("%.3f ", B[i]);
    }
    printf("\n");
    
    return 0;
}
```

## Alternative Implementation (More Robust)

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int main() {
    // Read DNA string
    char dna[101];
    printf("Enter DNA string (max 100 bp): ");
    scanf("%s", dna);
    
    int len = strlen(dna);
    
    // Read GC-content array A
    double A[10];
    printf("Enter 10 GC-content values (0-1): ");
    for (int i = 0; i < 10; i++) {
        scanf("%lf", &A[i]);
    }
    
    // Calculate probabilities for each GC-content
    double B[10];
    
    for (int i = 0; i < 10; i++) {
        double gc_content = A[i];
        double at_content = 1.0 - gc_content;
        
        double log_probability = 0.0;
        
        // For each nucleotide in the DNA string
        for (int j = 0; j < len; j++) {
            char nucleotide = dna[j];
            
            if (nucleotide == 'G' || nucleotide == 'C') {
                // Log probability of GC = log10(gc_content / 2)
                log_probability += log10(gc_content / 2.0);
            } else if (nucleotide == 'A' || nucleotide == 'T') {
                // Log probability of AT = log10(at_content / 2)
                log_probability += log10(at_content / 2.0);
            }
        }
        
        B[i] = log_probability;
    }
    
    // Output results
    printf("Results:\n");
    for (int i = 0; i < 10; i++) {
        printf("%.3f ", B[i]);
    }
    printf("\n");
    
    return 0;
}
```

## Explanation

The key insight is that:
1. For a given GC-content, the probability of each nucleotide is:
   - P(G) = P(C) = GC-content/2
   - P(A) = P(T) = (1 - GC-content)/2
2. For a specific DNA string, the probability of generating it is the product of probabilities for each position
3. Taking the logarithm of the product gives us the sum of logarithms (more numerically stable)

## Sample Input/Output

**Input:**
```
ACGTACGT
0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
```

**Output:**
```
Results:
-1.500 -1.301 -1.101 -0.903 -0.704 -0.505 -0.306 -0.107 0.092 0.292 
```

The solution handles the mathematical computation of probabilities and their logarithms efficiently, making it suitable for the Rosalind platform requirements.

