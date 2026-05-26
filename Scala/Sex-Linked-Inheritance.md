# Rosalind Problem: Sex-Linked_Inheritance

## Problem Description
In sex-linked inheritance, genes are located on the X chromosome. Males have one X and one Y chromosome (XY), while females have two X chromosomes (XX). For a recessive trait to be expressed in males, it must be present on their single X chromosome. For females, both X chromosomes must carry the recessive allele.

## Solution Approach
We need to calculate the probability that a female offspring will express a recessive sex-linked trait given:
- The probability that a male parent carries the recessive allele
- The probability that a female parent carries the recessive allele

## Scala Implementation

```scala
object SexLinkedInheritance {
  
  /**
   * Calculate the probability that a female offspring will express a recessive sex-linked trait
   * 
   * @param maleCarryProb - Probability that male parent carries the recessive allele
   * @param femaleCarryProb - Probability that female parent carries the recessive allele
   * @return Probability that female offspring expresses the trait
   */
  def sexLinkedInheritance(maleCarryProb: Double, femaleCarryProb: Double): Double = {
    // For a female to express a recessive trait, she needs two copies of the recessive allele
    // One from each parent
    maleCarryProb * femaleCarryProb
  }
  
  /**
   * Alternative implementation for the specific case where we have 3 values
   * representing the number of dominant alleles in each parent
   * 
   * @param domMale - Number of dominant alleles in male parent (0, 1, or 2)
   * @param domFemale - Number of dominant alleles in female parent (0, 1, or 2)
   * @return Probability that female offspring expresses recessive trait
   */
  def sexLinkedInheritanceFromCounts(domMale: Int, domFemale: Int): Double = {
    // Male has 1 X chromosome, so probability of carrying recessive allele = (2 - domMale) / 2
    val maleCarryProb = (2.0 - domMale) / 2.0
    
    // Female has 2 X chromosomes, so probability of carrying recessive allele = (2 - domFemale) / 2
    val femaleCarryProb = (2.0 - domFemale) / 2.0
    
    maleCarryProb * femaleCarryProb
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage:
    // If we have 3 values representing the number of dominant alleles in parents
    // This would be used for the specific Rosalind problem format
    
    // For example, if we have 1 dominant allele in male and 2 dominant alleles in female:
    val result1 = sexLinkedInheritanceFromCounts(1, 2)
    println(s"Probability: $result1")
    
    // If we have probabilities directly:
    val result2 = sexLinkedInheritance(0.5, 0.3)
    println(s"Probability: $result2")
  }
}

// For the specific Rosalind problem format, we might have:
object RosalindSolution {
  
  def solve(input: String): Double = {
    // Parse input - assuming format like "1 2 3" representing counts
    val counts = input.trim.split(" ").map(_.toInt)
    
    if (counts.length >= 2) {
      val domMale = counts(0)
      val domFemale = counts(1)
      
      // Convert to probabilities
      val maleCarryProb = (2.0 - domMale) / 2.0
      val femaleCarryProb = (2.0 - domFemale) / 2.0
      
      maleCarryProb * femaleCarryProb
    } else {
      0.0
    }
  }
  
  def main(args: Array[String]): Unit = {
    // Example input: "1 2" 
    // Male has 1 dominant allele (so 1 recessive), Female has 2 dominant alleles (so 0 recessive)
    val input = "1 2"
    val result = solve(input)
    println(f"Result: %.6f".format(result))
  }
}
```

## Explanation

The solution works as follows:

1. **Understanding Sex-Linked Inheritance**: 
   - Males (XY) have only one X chromosome, so if they carry a recessive allele, they express the trait
   - Females (XX) need two copies of the recessive allele to express the trait

2. **Probability Calculation**:
   - For a female to express a recessive trait, she must inherit the recessive allele from both parents
   - If the probability that a male carries the recessive allele is `p_male`, and female is `p_female`
   - Then the probability that a female offspring expresses the trait is `p_male × p_female`

3. **Input Conversion**:
   - If given counts of dominant alleles, we convert to probabilities:
   - Male: probability of carrying recessive = (2 - dominant_count) / 2
   - Female: probability of carrying recessive = (2 - dominant_count) / 2

## Example
If male parent has 1 dominant allele (so 1 recessive) and female parent has 2 dominant alleles (so 0 recessive):
- Male carry probability = (2-1)/2 = 0.5
- Female carry probability = (2-2)/2 = 0
- Result = 0.5 × 0 = 0

This makes sense because if the female parent doesn't carry the recessive allele, she can't pass it on.

