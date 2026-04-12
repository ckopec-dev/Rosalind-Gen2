# Rosalind Problem: Sex-Linked_Inheritance (F# Solution)

## Problem Understanding

In sex-linked inheritance, genes located on the X chromosome are inherited differently in males and females. Males (XY) have only one X chromosome, while females (XX) have two. This affects the probability of inheriting certain traits.

## Solution Approach

We need to calculate the probability that a trait is expressed in offspring given sex-linked inheritance patterns.

```fsharp
// Sex-Linked_Inheritance Solution in F#

open System

// Function to calculate probability of sex-linked inheritance
let sexLinkedInheritance (x1: float) (x2: float) : float =
    // x1: probability that mother is carrier (X^A X^a)
    // x2: probability that father is affected (X^a Y)
    // For sex-linked inheritance, we need to consider:
    // - Mother can pass X^A or X^a to offspring
    // - Father always passes X^a to daughters and Y to sons
    
    // Probability that daughter is affected (X^a X^a)
    // Daughter gets X^a from father, needs X^a from mother
    // P(daughter affected) = x1 * 0.5
    
    // Probability that son is affected (X^a Y)
    // Son gets X^a from mother (x1 * 0.5) or X^A from mother (0.5 * (1-x1))
    // But since father always passes X^a, son is affected if mother passes X^a
    // P(son affected) = x1 * 0.5
    
    // For the problem, we typically want the probability that offspring is affected
    // Let's assume we want the probability that a randomly selected offspring is affected
    
    let probDaughterAffected = x1 * 0.5
    let probSonAffected = x1 * 0.5
    
    // If we want the overall probability (assuming equal chance of male/female)
    let overallProbAffected = 0.5 * probDaughterAffected + 0.5 * probSonAffected
    
    overallProbAffected

// Alternative interpretation - specific case
let sexLinkedInheritanceSpecific (motherCarrier: float) (fatherAffected: bool) : float =
    // If mother is carrier (X^A X^a) and father is affected (X^a Y)
    // Probability that offspring is affected
    
    if fatherAffected then
        // Father is affected (X^a Y)
        // Daughter gets X^a from father, X^a from mother (if mother is carrier)
        // Son gets X^a from father (always affected)
        motherCarrier * 0.5  // Probability son is affected
    else
        0.0  // Father not affected, so no X^a to pass

// More general function for calculating sex-linked inheritance probabilities
let calculateSexLinkedProbabilities (pMotherCarrier: float) : float * float =
    // Returns (probability daughter affected, probability son affected)
    
    // Daughter gets X^a from father (always)
    // Daughter gets X^a from mother with probability pMotherCarrier * 0.5
    // Daughter is affected if she gets X^a from both parents
    let probDaughterAffected = pMotherCarrier * 0.5
    
    // Son gets X^a from father (always)
    // Son is affected if he gets X^a from father (always) - but that's wrong
    // Actually, son is affected if he gets X^a from father (always) and father is affected
    // Wait, let me re-read the problem...
    
    // Let's assume father is affected (X^a Y) - this is typical for sex-linked recessive
    // Son gets X^a from father (always) and X^a from mother (with probability pMotherCarrier * 0.5)
    // So son is affected if he gets X^a from mother (which is pMotherCarrier * 0.5)
    let probSonAffected = pMotherCarrier * 0.5
    
    (probDaughterAffected, probSonAffected)

// Example usage
let solveSexLinkedInheritance (motherCarrier: float) : float =
    // Assuming father is affected (X^a Y)
    // Probability that offspring is affected
    motherCarrier * 0.5

// Main function to solve the problem
let solveRosalindSexLinkedInheritance (input: string) : string =
    // Parse input - typically this would be probabilities
    let values = input.Split([|' '; '\t'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    
    if values.Length >= 1 then
        let p = float values.[0]
        let result = solveSexLinkedInheritance p
        sprintf "%.6f" result
    else
        "0.000000"

// Test cases
let testSexLinkedInheritance () =
    printfn "Test 1: Mother carrier = 0.5"
    let result1 = solveSexLinkedInheritance 0.5
    printfn "Result: %s" result1
    
    printfn "Test 2: Mother carrier = 0.25"
    let result2 = solveSexLinkedInheritance 0.25
    printfn "Result: %s" result2
    
    printfn "Test 3: Mother carrier = 1.0"
    let result3 = solveSexLinkedInheritance 1.0
    printfn "Result: %s" result3

// Run tests
testSexLinkedInheritance()

// Example usage with Rosalind-style input
let exampleInput = "0.5"
let exampleOutput = solveRosalindSexLinkedInheritance exampleInput
printfn "Example input: %s" exampleInput
printfn "Example output: %s" exampleOutput
```

## Key Concepts

1. **Sex-Linked Inheritance**: Genes on X chromosome are inherited differently in males and females
2. **Males (XY)**: Have one X chromosome from mother, one Y chromosome from father
3. **Females (XX)**: Have one X chromosome from each parent
4. **Recessive Traits**: For a male to be affected, he only needs one copy of the recessive allele

## Mathematical Foundation

- If mother is a carrier (X^A X^a) and father is affected (X^a Y):
  - Probability son is affected = P(mother passes X^a) × P(father passes X^a) = 0.5 × 1 = 0.5
  - Probability daughter is affected = P(mother passes X^a) × P(father passes X^a) = 0.5 × 1 = 0.5

The solution assumes that the input represents the probability that the mother is a carrier of the recessive allele.

