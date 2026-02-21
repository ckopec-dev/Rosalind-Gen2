# Rosalind Problem: Compute the Probability of a Spectral Dictionary

## Problem Description
Given a string `s` and a spectral dictionary `d`, compute the probability that a random string of length `n` over the alphabet of `s` matches a string in `d`.

## Solution

```kotlin
fun computeProbabilityOfSpectralDictionary(s: String, d: List<String>): Double {
    // Count frequency of each character in string s
    val charCount = mutableMapOf<Char, Int>()
    for (char in s) {
        charCount[char] = charCount.getOrDefault(char, 0) + 1
    }
    
    // Calculate total number of characters
    val totalChars = s.length
    
    // Calculate probability for each character
    val charProbabilities = mutableMapOf<Char, Double>()
    for ((char, count) in charCount) {
        charProbabilities[char] = count.toDouble() / totalChars
    }
    
    // Calculate probability of each string in dictionary d
    val probabilities = mutableListOf<Double>()
    
    for (word in d) {
        var prob = 1.0
        for (char in word) {
            prob *= charProbabilities.getOrDefault(char, 0.0)
        }
        probabilities.add(prob)
    }
    
    // Sum all probabilities
    return probabilities.sum()
}

// Alternative implementation using more functional approach
fun computeProbabilityOfSpectralDictionaryFunctional(s: String, d: List<String>): Double {
    val charCount = s.groupingBy { it }.eachCount()
    val totalChars = s.length.toDouble()
    val charProbabilities = charCount.mapValues { (_, count) -> count / totalChars }
    
    return d.map { word ->
        word.mapNotNull { charProbabilities[it] }.reduceOrNull { acc, prob -> acc * prob } ?: 0.0
    }.sum()
}

// Example usage
fun main() {
    val s = "ACGT"
    val d = listOf("AC", "CG", "GT")
    
    val result = computeProbabilityOfSpectralDictionary(s, d)
    println("Probability: $result")
    
    // Test with example from Rosalind
    val s2 = "ACGTACGT"
    val d2 = listOf("AC", "CG", "GT")
    
    val result2 = computeProbabilityOfSpectralDictionary(s2, d2)
    println("Probability: $result2")
}
```

## Explanation

1. **Character Frequency Counting**: First, we count how many times each character appears in the string `s`.

2. **Probability Calculation**: For each character, we calculate its probability as `count/total_length`.

3. **Dictionary Word Probability**: For each word in the dictionary `d`, we calculate the probability of generating that word by multiplying the probabilities of each character in the word.

4. **Final Result**: We sum up all the probabilities from the dictionary words to get the total probability.

## Time and Space Complexity

- **Time Complexity**: O(n + m), where n is the length of string `s` and m is the total length of all words in dictionary `d`
- **Space Complexity**: O(k), where k is the number of unique characters in string `s`

## Key Points

- The solution assumes that characters are chosen independently and uniformly from the alphabet
- For each word in the dictionary, we calculate the probability of randomly generating that exact word
- The final probability is the sum of probabilities for all words in the dictionary
- Edge cases like empty strings or missing characters are handled by returning 0.0 probability for missing characters

This approach efficiently computes the probability that a random string of length equal to the dictionary words would match any of the given strings in the spectral dictionary.

