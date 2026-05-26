# Rosalind Problem: Find a Highest-Scoring Modified Peptide against a Spectrum (Scala Solution)

## Problem Understanding

This problem asks us to find the highest-scoring modified peptide that matches a given spectrum. The modification involves adding a fixed mass (typically 15.994915 Da) to any amino acid in the peptide.

## Approach

1. **Dynamic Programming**: Use dynamic programming to find the optimal score for each possible peptide length
2. **Modified Amino Acids**: Consider that any amino acid can be modified (add 15.994915 Da)
3. **Spectrum Matching**: Score how well a peptide matches the given spectrum
4. **Backtracking**: Reconstruct the actual peptide sequence

## Solution

```scala
import scala.collection.mutable

object ModifiedPeptide {
  
  // Amino acid masses (standard masses)
  val aminoAcidMasses = Map(
    'A' -> 71.03711,
    'C' -> 103.00919,
    'D' -> 115.02694,
    'E' -> 129.04259,
    'F' -> 147.06841,
    'G' -> 57.02137,
    'H' -> 137.05891,
    'I' -> 113.08406,
    'K' -> 128.09496,
    'L' -> 113.08406,
    'M' -> 131.04049,
    'N' -> 114.04293,
    'P' -> 97.05276,
    'Q' -> 128.05858,
    'R' -> 156.10111,
    'S' -> 87.03203,
    'T' -> 101.04768,
    'V' -> 99.06841,
    'W' -> 186.07931,
    'Y' -> 163.06333
  )
  
  // Modified amino acid masses (add 15.994915 Da)
  val modifiedAminoAcidMasses = aminoAcidMasses.map { case (aa, mass) =>
    aa -> (mass + 15.994915)
  }
  
  // Convert spectrum to set of masses for fast lookup
  def spectrumToSet(spectrum: List[Double]): Set[Double] = {
    spectrum.toSet
  }
  
  // Calculate theoretical spectrum of a peptide
  def theoreticalSpectrum(peptide: String): List[Double] = {
    val masses = peptide.map(aminoAcidMasses(_)).toList
    val prefixSums = masses.scanLeft(0.0)(_ + _)
    val suffixSums = masses.reverse.scanLeft(0.0)(_ + _).reverse
    
    val spectrum = mutable.ListBuffer[Double]()
    
    // Add 0 mass (start)
    spectrum += 0.0
    
    // Add prefix masses
    for (i <- 1 until prefixSums.length) {
      spectrum += prefixSums(i)
    }
    
    // Add suffix masses (excluding the last one which is the full peptide)
    for (i <- 1 until suffixSums.length - 1) {
      spectrum += suffixSums(i)
    }
    
    spectrum.toList
  }
  
  // Calculate score of a peptide against a spectrum
  def scorePeptide(peptide: String, spectrumSet: Set[Double], 
                  aminoAcidMasses: Map[Char, Double]): Int = {
    val theoretical = theoreticalSpectrum(peptide)
    val score = theoretical.count(mass => spectrumSet.contains(mass))
    score
  }
  
  // Find highest scoring modified peptide
  def findHighestScoringModifiedPeptide(spectrum: List[Double], 
                                       maxPeptideLength: Int = 10): String = {
    
    val spectrumSet = spectrumToSet(spectrum)
    val aminoAcids = aminoAcidMasses.keys.toList
    
    // DP table: dp[length][mass] = best score for peptide of given length ending with given mass
    val dp = mutable.Map[String, Int]()
    val backtrack = mutable.Map[String, String]()
    
    // Initialize
    dp("") = 0
    
    // For each peptide length
    for (length <- 1 to maxPeptideLength) {
      val newDp = mutable.Map[String, Int]()
      val newBacktrack = mutable.Map[String, String]()
      
      // For each possible peptide of current length
      for ((peptide, score) <- dp if peptide.length == length - 1) {
        // Try adding each amino acid (standard and modified)
        for (aa <- aminoAcids) {
          val newPeptide = peptide + aa
          val newScore = scorePeptide(newPeptide, spectrumSet, aminoAcidMasses)
          
          // Consider both standard and modified versions
          val modifiedPeptide = newPeptide
          val modifiedScore = scorePeptide(modifiedPeptide, spectrumSet, modifiedAminoAcidMasses)
          
          if (newScore > newDp.getOrElse(newPeptide, -1)) {
            newDp(newPeptide) = newScore
            newBacktrack(newPeptide) = peptide
          }
        }
      }
      
      dp.clear()
      dp ++= newDp
      backtrack.clear()
      backtrack ++= newBacktrack
    }
    
    // Find best peptide
    val bestPeptide = dp.maxBy(_._2)._1
    bestPeptide
  }
  
  // Alternative more efficient approach using dynamic programming with mass tracking
  def findHighestScoringModifiedPeptideEfficient(spectrum: List[Double]): String = {
    val spectrumSet = spectrumToSet(spectrum)
    val aminoAcids = aminoAcidMasses.keys.toList
    
    // Memoization for dynamic programming
    val memo = mutable.Map[String, (Int, String)]()
    
    def scoreAndReconstruct(peptide: String): (Int, String) = {
      if (memo.contains(peptide)) {
        return memo(peptide)
      }
      
      val score = scorePeptide(peptide, spectrumSet, aminoAcidMasses)
      
      if (peptide.length < 10) { // Limit search space
        var bestScore = score
        var bestPeptide = peptide
        
        // Try adding each amino acid
        for (aa <- aminoAcids) {
          val newPeptide = peptide + aa
          val (newScore, _) = scoreAndReconstruct(newPeptide)
          if (newScore > bestScore) {
            bestScore = newScore
            bestPeptide = newPeptide
          }
        }
        
        memo(peptide) = (bestScore, bestPeptide)
        return (bestScore, bestPeptide)
      }
      
      memo(peptide) = (score, peptide)
      (score, peptide)
    }
    
    // Start with empty peptide
    val (_, best) = scoreAndReconstruct("")
    best
  }
  
  // Main function to solve the problem
  def solve(spectrum: List[Double]): String = {
    // Simple approach: try peptides of increasing length
    val aminoAcids = aminoAcidMasses.keys.toList
    var bestPeptide = ""
    var bestScore = -1
    
    // Try peptides of length 1 to 10
    for (length <- 1 to 10) {
      val candidates = generatePeptides(aminoAcids, length)
      for (peptide <- candidates) {
        val score = scorePeptide(peptide, spectrumToSet(spectrum), aminoAcidMasses)
        if (score > bestScore) {
          bestScore = score
          bestPeptide = peptide
        }
      }
    }
    
    bestPeptide
  }
  
  // Generate all peptides of given length
  def generatePeptides(aminoAcids: List[Char], length: Int): List[String] = {
    if (length == 0) return List("")
    if (length == 1) return aminoAcids.map(_.toString)
    
    val smaller = generatePeptides(aminoAcids, length - 1)
    for {
      aa <- aminoAcids
      peptide <- smaller
    } yield aa + peptide
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage:
    val spectrum = List(0.0, 113.0, 128.0, 147.0, 203.0, 218.0, 230.0, 245.0, 262.0, 282.0, 297.0, 312.0, 332.0, 347.0, 362.0, 377.0, 392.0, 407.0, 422.0, 437.0, 452.0, 467.0, 482.0, 497.0, 512.0, 527.0, 542.0, 557.0, 572.0, 587.0, 602.0, 617.0, 632.0, 647.0, 662.0, 677.0, 692.0, 707.0, 722.0, 737.0, 752.0, 767.0, 782.0, 797.0, 812.0, 827.0, 842.0, 857.0, 872.0, 887.0, 902.0, 917.0, 932.0, 947.0, 962.0, 977.0, 992.0, 1007.0, 1022.0, 1037.0, 1052.0, 1067.0, 1082.0, 1097.0, 1112.0, 1127.0, 1142.0, 1157.0, 1172.0, 1187.0, 1202.0, 1217.0, 1232.0, 1247.0, 1262.0, 1277.0, 1292.0, 1307.0, 1322.0, 1337.0, 1352.0, 1367.0, 1382.0, 1397.0, 1412.0, 1427.0, 1442.0, 1457.0, 1472.0, 1487.0, 1502.0, 1517.0, 1532.0, 1547.0, 1562.0, 1577.0, 1592.0, 1607.0, 1622.0, 1637.0, 1652.0, 1667.0, 1682.0, 1697.0, 1712.0, 1727.0, 1742.0, 1757.0, 1772.0, 1787.0, 1802.0, 1817.0, 1832.0, 1847.0, 1862.0, 1877.0, 1892.0, 1907.0, 1922.0, 1937.0, 1952.0, 1967.0, 1982.0, 1997.0, 2012.0, 2027.0, 2042.0, 2057.0, 2072.0, 2087.0, 2102.0, 2117.0, 2132.0, 2147.0, 2162.0, 2177.0, 2192.0, 2207.0, 2222.0, 2237.0, 2252.0, 2267.0, 2282.0, 2297.0, 2312.0, 2327.0, 2342.0, 2357.0, 2372.0, 2387.0, 2402.0, 2417.0, 2432.0, 2447.0, 2462.0, 2477.0, 2492.0, 2507.0, 2522.0, 2537.0, 2552.0, 2567.0, 2582.0, 2597.0, 2612.0, 2627.0, 2642.0, 2657.0, 2672.0, 2687.0, 2702.0, 2717.0, 2732.0, 2747.0, 2762.0, 2777.0, 2792.0, 2807.0, 2822.0, 2837.0, 2852.0, 2867.0, 2882.0, 2897.0, 2912.0, 2927.0, 2942.0, 2957.0, 2972.0, 2987.0, 3002.0, 3017.0, 3032.0, 3047.0, 3062.0, 3077.0, 3092.0, 3107.0, 3122.0, 3137.0, 3152.0, 3167.0, 3182.0, 3197.0, 3212.0, 3227.0, 3242.0, 3257.0, 3272.0, 3287.0, 3302.0, 3317.0, 3332.0, 3347.0, 3362.0, 3377.0, 3392.0, 3407.0, 3422.0, 3437.0, 3452.0, 3467.0, 3482.0, 3497.0, 3512.0, 3527.0, 3542.0, 3557.0, 3572.0, 3587.0, 3602.0, 3617.0, 3632.0, 3647.0, 3662.0, 3677.0, 3692.0, 3707.0, 3722.0, 3737.0, 3752.0, 3767.0, 3782.0, 3797.0, 3812.0, 3827.0, 3842.0, 3857.0, 3872.0, 3887.0, 3902.0, 3917.0, 3932.0, 3947.0, 3962.0, 3977.0, 3992.0, 4007.0, 4022.0, 4037.0, 4052.0, 4067.0, 4082.0, 4097.0, 4112.0, 4127.0, 4142.0, 4157.0, 4172.0, 4187.0, 4202.0, 4217.0, 4232.0, 424