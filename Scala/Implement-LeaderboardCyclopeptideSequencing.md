# Rosalind Problem: Implement LeaderboardCyclopeptideSequencing

## Problem Description
Implement the LeaderboardCyclopeptideSequencing algorithm to find the cyclic peptide with the best score that matches the given spectrum.

## Solution in Scala

```scala
object LeaderboardCyclopeptideSequencing {
  
  // Amino acid masses
  val MASS_TABLE = Map(
    'G' -> 57, 'A' -> 71, 'S' -> 87, 'P' -> 97, 'V' -> 99,
    'T' -> 101, 'C' -> 103, 'I' -> 113, 'L' -> 113, 'N' -> 114,
    'D' -> 115, 'K' -> 128, 'Q' -> 128, 'E' -> 129, 'M' -> 131,
    'H' -> 137, 'F' -> 147, 'R' -> 156, 'Y' -> 163, 'W' -> 186
  )
  
  val MASS_VALUES = MASS_TABLE.values.toList.sorted
  
  def cyclospectrum(peptide: String): List[Int] = {
    val prefixMass = peptide.map(MASS_TABLE(_)).scanLeft(0)(_ + _)
    val peptideMass = prefixMass.last
    val spectrum = List(0)
    
    (0 until peptide.length).flatMap { i =>
      (i + 1 until peptide.length).map { j =>
        val mass = prefixMass(j + 1) - prefixMass(i)
        mass
      } :+ (peptideMass - (prefixMass(j + 1) - prefixMass(i)))
    } :+ peptideMass
  }
  
  def score(peptide: String, spectrum: List[Int]): Int = {
    val peptideSpectrum = cyclospectrum(peptide)
    val spectrumCounts = spectrum.groupBy(identity).mapValues(_.length)
    val peptideSpectrumCounts = peptideSpectrum.groupBy(identity).mapValues(_.length)
    
    peptideSpectrumCounts.foldLeft(0) { case (score, (mass, count)) =>
      val spectrumCount = spectrumCounts.getOrElse(mass, 0)
      score + math.min(count, spectrumCount)
    }
  }
  
  def expand(peptides: List[String], aminoAcids: List[Char]): List[String] = {
    peptides.flatMap(peptide => aminoAcids.map(aa => peptide + aa))
  }
  
  def trim(leaderboard: List[String], spectrum: List[Int], n: Int): List[String] = {
    if (leaderboard.isEmpty) return List.empty
    
    val scores = leaderboard.map(peptide => (peptide, score(peptide, spectrum)))
    val sortedScores = scores.sortBy(-_._2)
    val topN = sortedScores.take(n)
    
    // Handle ties - if multiple peptides have the same score, include all
    val maxScore = topN.head._2
    val trimmed = topN.takeWhile(_._2 == maxScore)
    
    trimmed.map(_._1)
  }
  
  def leaderboardCyclopeptideSequencing(spectrum: List[Int], n: Int): String = {
    var leaderboard = List("")
    var leaderPeptide = ""
    var leaderScore = 0
    
    while (leaderboard.nonEmpty) {
      leaderboard = expand(leaderboard, MASS_TABLE.keys.toList)
      
      // Remove peptides with masses greater than the target
      val targetMass = spectrum.last
      leaderboard = leaderboard.filter { peptide =>
        val peptideMass = peptide.map(MASS_TABLE(_)).sum
        peptideMass <= targetMass
      }
      
      // Score all peptides in leaderboard
      leaderboard = trim(leaderboard, spectrum, n)
      
      // Remove peptides with masses greater than the target
      leaderboard = leaderboard.filter { peptide =>
        val peptideMass = peptide.map(MASS_TABLE(_)).sum
        peptideMass <= targetMass
      }
      
      // Update leader if needed
      leaderboard.foreach { peptide =>
        val peptideMass = peptide.map(MASS_TABLE(_)).sum
        if (peptideMass == targetMass) {
          val peptideScore = score(peptide, spectrum)
          if (peptideScore > leaderScore) {
            leaderScore = peptideScore
            leaderPeptide = peptide
          }
        }
      }
    }
    
    leaderPeptide
  }
  
  // Alternative implementation with better performance
  def leaderboardCyclopeptideSequencingOptimized(spectrum: List[Int], n: Int): String = {
    var leaderboard = List("")
    var leaderPeptide = ""
    var leaderScore = 0
    
    val targetMass = spectrum.last
    
    while (leaderboard.nonEmpty) {
      leaderboard = expand(leaderboard, MASS_TABLE.keys.toList)
      
      // Filter out peptides that are too long
      leaderboard = leaderboard.filter { peptide =>
        val peptideMass = peptide.map(MASS_TABLE(_)).sum
        peptideMass <= targetMass
      }
      
      // Score all peptides in leaderboard
      val scores = leaderboard.map(peptide => (peptide, score(peptide, spectrum)))
      
      // Sort by score and trim
      val sortedScores = scores.sortBy(-_._2)
      leaderboard = sortedScores.take(n).map(_._1)
      
      // Update leader if needed
      leaderboard.foreach { peptide =>
        val peptideMass = peptide.map(MASS_TABLE(_)).sum
        if (peptideMass == targetMass) {
          val peptideScore = score(peptide, spectrum)
          if (peptideScore > leaderScore) {
            leaderScore = peptideScore
            leaderPeptide = peptide
          }
        }
      }
    }
    
    leaderPeptide
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val spectrum = List(0, 71, 113, 129, 147, 203, 221, 237, 257, 313, 331, 347, 365, 421, 439, 460, 482, 505, 527, 543, 569, 625, 659, 671, 689, 701, 717, 735, 752, 770, 786, 803, 821, 837, 853, 871, 889, 907, 923, 941, 959, 977, 995, 1013, 1031, 1049, 1067, 1085, 1103, 1121, 1139, 1157, 1175, 1193, 1211, 1229, 1247, 1265, 1283, 1301, 1319, 1337, 1355, 1373, 1391, 1409, 1427, 1445, 1463, 1481, 1499, 1517, 1535, 1553, 1571, 1589, 1607, 1625, 1643, 1661, 1679, 1697, 1715, 1733, 1751, 1769, 1787, 1805, 1823, 1841, 1859, 1877, 1895, 1913, 1931, 1949, 1967, 1985, 2003, 2021, 2039, 2057, 2075, 2093, 2111, 2129, 2147, 2165, 2183, 2201, 2219, 2237, 2255, 2273, 2291, 2309, 2327, 2345, 2363, 2381, 2399, 2417, 2435, 2453, 2471, 2489, 2507, 2525, 2543, 2561, 2579, 2597, 2615, 2633, 2651, 2669, 2687, 2705, 2723, 2741, 2759, 2777, 2795, 2813, 2831, 2849, 2867, 2885, 2903, 2921, 2939, 2957, 2975, 2993, 3011, 3029, 3047, 3065, 3083, 3101, 3119, 3137, 3155, 3173, 3191, 3209, 3227, 3245, 3263, 3281, 3299, 3317, 3335, 3353, 3371, 3389, 3407, 3425, 3443, 3461, 3479, 3497, 3515, 3533, 3551, 3569, 3587, 3605, 3623, 3641, 3659, 3677, 3695, 3713, 3731, 3749, 3767, 3785, 3803, 3821, 3839, 3857, 3875, 3893, 3911, 3929, 3947, 3965, 3983, 4001, 4019, 4037, 4055, 4073, 4091, 4109, 4127, 4145, 4163, 4181, 4199, 4217, 4235, 4253, 4271, 4289, 4307, 4325, 4343, 4361, 4379, 4397, 4415, 4433, 4451, 4469, 4487, 4505, 4523, 4541, 4559, 4577, 4595, 4613, 4631, 4649, 4667, 4685, 4703, 4721, 4739, 4757, 4775, 4793, 4811, 4829, 4847, 4865, 4883, 4901, 4919, 4937, 4955, 4973, 4991, 5009, 5027, 5045, 5063, 5081, 5099, 5117, 5135, 5153, 5171, 5189, 5207, 5225, 5243, 5261, 5279, 5297, 5315, 5333, 5351, 5369, 5387, 5405, 5423, 5441, 5459, 5477, 5495, 5513, 5531, 5549, 5567, 5585, 5603, 5621, 5639, 5657, 5675, 5693, 5711, 5729, 5747, 5765, 5783, 5801, 5819, 5837, 5855, 5873, 5891, 5909, 5927, 5945, 5963, 5981, 5999, 6017, 6035, 6053, 6071, 6089, 6107, 6125, 6143, 6161, 6179, 6197, 6215, 6233, 6251, 6269, 6287, 6305, 6323, 6341, 6359, 6377, 6395, 6413, 6431, 6449, 6467, 6485, 6503, 6521, 6539, 6557, 6575, 6593, 6611, 6629, 6647, 6665, 6683, 6701, 6719, 6737, 6755, 6773, 6791, 6809, 6827, 6845, 6863, 6881, 6899, 6917, 6935, 6953, 6971, 6989, 7007, 7025, 7043, 7061, 7079, 7097, 7115, 7133, 7151, 7169, 7187, 7205, 7223, 7241, 7259, 7277, 7295, 7313, 7331, 7349, 7367, 7385, 7403, 7421, 7439, 7457, 7475, 7493, 7511, 7529, 7547, 7565, 7583, 7601, 7619, 7637, 7655, 7673, 7691, 7709, 7727, 7745, 7763, 7781, 7799, 7817, 7835, 7853, 7871, 7889, 7907, 7925, 7943, 7961, 7979, 7997, 8015, 8033, 8051, 8069, 8087, 8105, 8123, 8141, 8159, 8177, 8195, 8213, 8231, 8249, 8267, 8285, 8303, 8321, 8339, 8357, 8375, 8393, 8411, 8429, 8447, 8465, 8483, 8501, 8519, 8537, 8555, 8573, 8591, 8609, 8627, 8645, 8663, 8681, 8699, 8717, 8735, 8753, 8771, 8789, 8807, 8825, 8843, 8861, 8879, 8897, 8915, 8933, 8951, 8969, 8987, 9005, 9023, 9041, 9059, 9077, 9095, 9113, 9131, 9149, 9167, 9185, 9203, 9221, 9239, 9257, 9275, 9293, 9311, 9329, 9347, 9365, 9383, 9401, 9419, 9437, 9455, 9473, 9491, 9509, 9527, 9545, 9563, 9581, 9599, 9617, 9635, 9653, 9671, 9689, 9707, 9725,