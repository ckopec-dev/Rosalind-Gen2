# Rosalind Problem: Perfect Matchings and RNA Secondary Structures

I'll solve this step by step using Haskell.

## Problem Understanding

The problem asks us to find the number of perfect matchings in an RNA secondary structure where:
- The RNA sequence has equal numbers of A/U and G/C nucleotides
- Perfect matching means every nucleotide is paired with exactly one other nucleotide
- We need to count all possible perfect matchings

## Approach

This is a combinatorial problem that can be solved using the following approach:
1. Count the number of A/U nucleotides (let's call it `n1`)
2. Count the number of G/C nucleotides (let's call it `n2`)
3. The number of perfect matchings is `n1! × n2!` (since we can pair A/U with G/C in any order)

Wait, that's not quite right. Let me reconsider...

Actually, this is about counting perfect matchings in a graph where:
- Each A can pair with a U
- Each G can pair with a C
- We want to count all possible ways to form perfect matchings

The correct approach is:
1. Count A's and U's separately (let's call them `a` and `u`)
2. Count G's and C's separately (let's call them `g` and `c`)
3. The number of perfect matchings is `a! × g!` (since we can arrange A's with U's and G's with C's independently)

Actually, let me think more carefully about the problem statement...

Looking at this more carefully, this is asking for the number of perfect matchings in a secondary structure, which means we're counting the number of ways to pair nucleotides such that:
- A pairs with U (and vice versa)
- G pairs with C (and vice versa)
- Each nucleotide appears in exactly one pair

The answer is `a! × g!` where `a` is the count of A's and U's, and `g` is the count of G's and C's.

## Haskell Solution

```haskell
import Data.List (group)
import qualified Data.Map as Map

-- Function to count nucleotides
countNucleotides :: String -> Map.Map Char Int
countNucleotides = foldr (\c acc -> Map.insertWith (+) c 1 acc) Map.empty

-- Function to calculate factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Function to count perfect matchings
perfectMatchings :: String -> Int
perfectMatchings seq = 
    let counts = countNucleotides seq
        aCount = Map.findWithDefault 0 'A' counts
        uCount = Map.findWithDefault 0 'U' counts
        gCount = Map.findWithDefault 0 'G' counts
        cCount = Map.findWithDefault 0 'C' counts
        auCount = aCount + uCount  -- Total A/U count
        gcCount = gCount + cCount  -- Total G/C count
    in factorial auCount * factorial gcCount

-- Alternative implementation using the more precise approach
perfectMatchings' :: String -> Int
perfectMatchings' seq = 
    let counts = countNucleotides seq
        aCount = Map.findWithDefault 0 'A' counts
        uCount = Map.findWithDefault 0 'U' counts
        gCount = Map.findWithDefault 0 'G' counts
        cCount = Map.findWithDefault 0 'C' counts
        -- For perfect matching, we need equal numbers of complementary pairs
        -- But the problem is asking for the number of ways to match
        -- If we have n A's and n U's, and m G's and m C's, 
        -- then we can arrange the pairings in n! × m! ways
        auPairs = min aCount uCount
        gcPairs = min gCount cCount
    in factorial auPairs * factorial gcPairs

-- Simpler and more correct approach
perfectMatchings'' :: String -> Int
perfectMatchings'' seq = 
    let counts = countNucleotides seq
        aCount = Map.findWithDefault 0 'A' counts
        uCount = Map.findWithDefault 0 'U' counts
        gCount = Map.findWithDefault 0 'G' counts
        cCount = Map.findWithDefault 0 'C' counts
        -- The number of perfect matchings is the product of factorials of 
        -- the minimum of complementary pairs
        au = min aCount uCount
        gc = min gCount cCount
    in factorial au * factorial gc

-- More elegant version
perfectMatchingsFinal :: String -> Int
perfectMatchingsFinal seq = 
    let counts = countNucleotides seq
        a = Map.findWithDefault 0 'A' counts
        u = Map.findWithDefault 0 'U' counts
        g = Map.findWithDefault 0 'G' counts
        c = Map.findWithDefault 0 'C' counts
        au = min a u
        gc = min g c
    in factorial au * factorial gc

-- Test function
main :: IO ()
main = do
    -- Example test case
    let testSeq = "AUGCUAGU"
    putStrLn $ "Input sequence: " ++ testSeq
    putStrLn $ "Perfect matchings: " ++ show (perfectMatchingsFinal testSeq)
    
    -- Another test case
    let testSeq2 = "AUCG"
    putStrLn $ "Input sequence: " ++ testSeq2
    putStrLn $ "Perfect matchings: " ++ show (perfectMatchingsFinal testSeq2)
```

Wait, let me reconsider the problem. Looking at this more carefully, I think I misunderstood the combinatorial nature.

The correct interpretation is that for a perfect matching:
- We have n A's that must pair with n U's (in n! ways)  
- We have m G's that must pair with m C's (in m! ways)
- But since we're pairing, we're looking at the number of ways to arrange the pairings

Actually, let me think of it this way:
If we have a sequence where we can pair A with U and G with C, and we want perfect matchings, we're essentially asking:
- How many ways can we pair n A's with n U's? That's n! ways
- How many ways can we pair m G's with m C's? That's m! ways
- Total = n! × m!

But this is wrong too. The problem is asking for the number of perfect matchings in a complete graph where we have to match complementary nucleotides.

Let me read the problem again and solve it properly:

```haskell
import qualified Data.Map as Map

-- Calculate factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Count nucleotides and compute perfect matchings
perfectMatchings :: String -> Int
perfectMatchings seq = 
    let counts = Map.fromListWith (+) [(c, 1) | c <- seq]
        a = Map.findWithDefault 0 'A' counts
        u = Map.findWithDefault 0 'U' counts
        g = Map.findWithDefault 0 'G' counts
        c = Map.findWithDefault 0 'C' counts
        -- For perfect matching, we need equal numbers of complementary pairs
        -- The number of ways to pair n A's with n U's is n!
        -- The number of ways to pair m G's with m C's is m!
        auPairs = min a u
        gcPairs = min g c
    in factorial auPairs * factorial gcPairs

-- Main function to solve the problem
solvePerfectMatchings :: String -> Int
solvePerfectMatchings = perfectMatchings

-- Example usage
main :: IO ()
main = do
    let sequence = "AUGCUAGU"
    let result = solvePerfectMatchings sequence
    putStrLn $ "Sequence: " ++ sequence
    putStrLn $ "Perfect matchings: " ++ show result
```

This is still not quite right. Let me think about the actual problem.

Actually, for the perfect matchings of an RNA sequence, the answer is:
- Count A's and U's: let's call this `n1`
- Count G's and C's: let's call this `n2`  
- The number of perfect matchings is `n1! × n2!`

But wait, that's not correct either. The problem is asking for the number of ways to form a perfect matching in the structure.

Looking at the problem more carefully - if we have a sequence where we can form a perfect matching, we're looking for the number of ways to pair complementary nucleotides.

The correct approach is to count:
- The number of A's and U's (let's say n1 each)  
- The number of G's and C's (let's say n2 each)
- Then the number of perfect matchings = n1! × n2!

```haskell
import qualified Data.Map as Map

-- Calculate factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Count nucleotides and compute perfect matchings
perfectMatchings :: String -> Int
perfectMatchings seq = 
    let counts = Map.fromListWith (+) [(c, 1) | c <- seq]
        a = Map.findWithDefault 0 'A' counts
        u = Map.findWithDefault 0 'U' counts
        g = Map.findWithDefault 0 'G' counts
        c = Map.findWithDefault 0 'C' counts
        -- For a perfect matching, we need to pair A with U and G with C
        -- The number of ways to arrange these pairings is:
        -- (number of A's)! * (number of G's)! 
        -- But this is only valid if we have equal numbers of A/U and G/C
        -- Actually, it's the number of ways to pair n A's with n U's = n!
        -- and n G's with n C's = n!
        au = min a u
        gc = min g c
    in factorial au * factorial gc

-- Main solution function
main :: IO ()
main = do
    -- Read input from stdin or use test case
    let testSequence = "AUGCUAGU"
    let result = perfectMatchings testSequence
    putStrLn $ "Input: " ++ testSequence
    putStrLn $ "Perfect matchings: " ++ show result
```

This should be the correct solution for the Rosalind problem Perfect_Matchings_and_RNA_Secondary_Structures.

