# Rosalind Problem: Implement_PSMSearch in Scala

## Problem Description
Implement a peptide spectrum matching (PSM) search algorithm that finds the best matching peptide for a given spectrum.

## Solution

```scala
object PSM {
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val spectrum = Array(0.0, 113.0, 128.0, 147.0, 163.0, 186.0, 229.0, 242.0, 257.0, 274.0, 305.0, 320.0, 333.0, 356.0, 369.0, 387.0, 400.0, 417.0, 430.0, 447.0, 460.0, 477.0, 490.0, 507.0, 520.0, 537.0, 550.0, 567.0, 580.0, 597.0, 610.0, 627.0, 640.0, 657.0, 670.0, 687.0, 700.0, 717.0, 730.0, 747.0, 760.0, 777.0, 790.0, 807.0, 820.0, 837.0, 850.0, 867.0, 880.0, 897.0, 910.0, 927.0, 940.0, 957.0, 970.0, 987.0, 1000.0)
    val peptides = Array("GAA", "GAG", "GAC", "GAT", "GGA", "GCG", "GCA", "GCT", "GTA", "GTC", "GTT", "GAGA", "GAGC", "GAGT", "GAGG")
    
    val bestMatch = psmSearch(spectrum, peptides)
    println(s"Best matching peptide: $bestMatch")
  }
  
  /**
   * Peptide Spectrum Matching Search
   * @param spectrum The experimental spectrum
   * @param peptides List of candidate peptides
   * @return The peptide with highest score
   */
  def psmSearch(spectrum: Array[Double], peptides: Array[String]): String = {
    val scores = peptides.map(peptide => (peptide, scorePeptide(spectrum, peptide)))
    scores.maxBy(_._2)._1
  }
  
  /**
   * Calculate score for a peptide against the spectrum
   * @param spectrum The experimental spectrum
   * @param peptide The candidate peptide
   * @return The matching score
   */
  def scorePeptide(spectrum: Array[Double], peptide: String): Double = {
    val theoreticalSpectrum = generateTheoreticalSpectrum(peptide)
    calculateScore(spectrum, theoreticalSpectrum)
  }
  
  /**
   * Generate theoretical spectrum for a peptide
   * @param peptide The peptide sequence
   * @return The theoretical spectrum
   */
  def generateTheoreticalSpectrum(peptide: String): Array[Double] = {
    val masses = Array('A' -> 71.03711, 'C' -> 103.00919, 'D' -> 115.02694, 'E' -> 129.04259,
                      'F' -> 147.06841, 'G' -> 57.02146, 'H' -> 137.05891, 'I' -> 113.08406,
                      'K' -> 128.09496, 'L' -> 113.08406, 'M' -> 131.04049, 'N' -> 114.04293,
                      'P' -> 97.05276, 'Q' -> 128.05858, 'R' -> 156.10111, 'S' -> 87.03203,
                      'T' -> 101.04768, 'V' -> 99.06841, 'W' -> 186.07931, 'Y' -> 163.06333)
    
    val peptideMasses = peptide.map(aa => masses.find(_._1 == aa).get._2)
    
    // Generate b-ions and y-ions
    val bIons = generateBions(peptideMasses)
    val yIons = generateYions(peptideMasses)
    
    // Combine and sort
    (bIons ++ yIons).sorted
  }
  
  /**
   * Generate b-ions for a peptide
   */
  def generateBions(peptideMasses: Array[Double]): Array[Double] = {
    val bIons = new scala.collection.mutable.ArrayBuffer[Double]()
    var cumulativeMass = 0.0
    for (i <- 0 until peptideMasses.length) {
      cumulativeMass += peptideMasses(i)
      bIons += cumulativeMass
    }
    bIons.toArray
  }
  
  /**
   * Generate y-ions for a peptide
   */
  def generateYions(peptideMasses: Array[Double]): Array[Double] = {
    val yIons = new scala.collection.mutable.ArrayBuffer[Double]()
    var cumulativeMass = 0.0
    for (i <- peptideMasses.length - 1 to 0 by -1) {
      cumulativeMass += peptideMasses(i)
      yIons += cumulativeMass
    }
    yIons.toArray
  }
  
  /**
   * Calculate matching score between experimental and theoretical spectra
   * @param experimental The experimental spectrum
   * @param theoretical The theoretical spectrum
   * @return The score
   */
  def calculateScore(experimental: Array[Double], theoretical: Array[Double]): Double = {
    // Simple scoring: count matching peaks within tolerance
    val tolerance = 1.0
    var score = 0.0
    
    for (expPeak <- experimental) {
      for (theoPeak <- theoretical) {
        if (math.abs(expPeak - theoPeak) <= tolerance) {
          score += 1.0
          // Break to avoid double counting
          // In a more sophisticated implementation, you might want to use a more complex scoring
          // like the dot product or other methods
          break
        }
      }
    }
    
    score
  }
  
  /**
   * Alternative scoring method using dot product
   */
  def dotProductScore(experimental: Array[Double], theoretical: Array[Double]): Double = {
    val tolerance = 1.0
    var score = 0.0
    
    for (expPeak <- experimental) {
      var bestMatch = 0.0
      for (theoPeak <- theoretical) {
        if (math.abs(expPeak - theoPeak) <= tolerance) {
          bestMatch = math.max(bestMatch, 1.0)
        }
      }
      score += bestMatch
    }
    
    score
  }
}
```

## Explanation

This implementation solves the PSM (Peptide Spectrum Matching) problem by:

1. **Generating Theoretical Spectra**: For each peptide, it generates both b-ions and y-ions using the standard amino acid masses.

2. **Scoring Function**: Compares the experimental spectrum with the theoretical spectrum to calculate a matching score.

3. **Best Match Selection**: Finds the peptide that gives the highest score.

### Key Components:

- **`generateTheoreticalSpectrum`**: Creates the theoretical spectrum from a peptide sequence
- **`generateBions`**: Calculates b-ion masses (prefix ions)
- **`generateYions`**: Calculates y-ion masses (suffix ions)
- **`calculateScore`**: Compares experimental and theoretical spectra
- **`psmSearch`**: Main function that finds the best matching peptide

### Time Complexity:
- O(n × m × k) where n is the number of peptides, m is the average peptide length, and k is the average number of peaks

### Space Complexity:
- O(m + k) for storing theoretical spectra and peaks

The algorithm uses a simple peak matching approach with a tolerance of 1.0 Da, but can be extended with more sophisticated scoring methods like dot products or other statistical measures.

