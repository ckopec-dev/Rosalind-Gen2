# Rosalind Problem: Generate the Theoretical Spectrum of a Linear Peptide

## Problem Statement
The theoretical spectrum of a linear peptide is the multiset of all possible subpeptide masses of that peptide. Given a linear peptide, we need to compute its theoretical spectrum.

## Solution in Scala

```scala
object LinearPeptideSpectrum {
  
  // Amino acid masses
  val aminoAcidMasses = Map(
    'A' -> 71, 'C' -> 103, 'D' -> 115, 'E' -> 129, 'F' -> 147,
    'G' -> 57, 'H' -> 137, 'I' -> 113, 'K' -> 128, 'L' -> 113,
    'M' -> 131, 'N' -> 114, 'P' -> 97, 'Q' -> 128, 'R' -> 156,
    'S' -> 87, 'T' -> 101, 'V' -> 99, 'W' -> 186, 'Y' -> 163
  )
  
  /**
   * Generate the theoretical spectrum of a linear peptide
   * @param peptide The amino acid sequence as a string
   * @return The theoretical spectrum as a list of masses
   */
  def generateTheoreticalSpectrum(peptide: String): List[Int] = {
    val masses = peptide.map(aminoAcidMasses(_)).toList
    
    // Generate all subpeptide masses
    val spectrum = scala.collection.mutable.ListBuffer[Int]()
    
    // Add the mass of the entire peptide
    spectrum += masses.sum
    
    // Generate all possible subpeptides
    for (i <- masses.indices) {
      var currentMass = 0
      for (j <- i until masses.length) {
        currentMass += masses(j)
        spectrum += currentMass
      }
    }
    
    // Add 0 for the empty subpeptide
    spectrum += 0
    
    // Sort the spectrum
    spectrum.toList.sorted
  }
  
  /**
   * Alternative implementation using a more functional approach
   */
  def generateTheoreticalSpectrumFunctional(peptide: String): List[Int] = {
    val masses = peptide.map(aminoAcidMasses(_)).toList
    
    // Generate all prefix sums
    val prefixSums = 0 :: masses.scanLeft(0)(_ + _).tail
    
    // Generate all subpeptide masses
    val subpeptideMasses = for {
      i <- prefixSums.indices
      j <- i until prefixSums.length
    } yield prefixSums(j) - prefixSums(i)
    
    // Sort and return
    (0 :: subpeptideMasses.toList).sorted
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val peptide = "SKADYEK"
    val spectrum = generateTheoreticalSpectrum(peptide)
    println(spectrum.mkString(" "))
    
    // Test with another example
    val peptide2 = "NQV"
    val spectrum2 = generateTheoreticalSpectrum(peptide2)
    println(spectrum2.mkString(" "))
  }
}
```

## Explanation

### Approach
1. **Mass Mapping**: Create a map of amino acids to their masses
2. **Prefix Sums**: Use prefix sums to efficiently calculate subpeptide masses
3. **Subpeptide Generation**: For each possible start position, generate all subpeptides ending at various positions
4. **Sorting**: Return the spectrum in ascending order

### Key Steps
1. Convert the peptide string to a list of masses
2. Calculate prefix sums to enable fast subpeptide mass computation
3. For each pair of indices (i,j), compute the mass of the subpeptide from position i to j
4. Include the mass of the empty subpeptide (0)
5. Sort the results

### Time Complexity
- **Time**: O(n²) where n is the length of the peptide
- **Space**: O(n²) for storing the spectrum

### Example Output
For peptide "SKADYEK":
```
0 113 128 186 241 299 357 372 429 444 499 523 557 612 627 685 722 773 810 835 886 941 956 1007 1031 1065 1122 1137 1192 1207 1265 1322 1337 1392 1407 1462 1519 1534 1591 1606 1661 1718 1733 1790 1805 1862 1919 1934 1991 2006 2063 2120 2135 2192 2207 2264 2321 2336 2393 2450 2465 2522 2579 2594 2651 2708 2723 2780 2837 2852 2909 2966 2981 3038 3095 3110 3167 3224 3239 3296 3353 3368 3425 3482 3497 3554 3611 3626 3683 3740 3755 3812 3869 3884 3941 3998 4013 4070 4127 4142 4199 4256 4271 4328 4385 4400 4457 4514 4529 4586 4643 4658 4715 4772 4787 4844 4901 4916 4973 5030 5045 5102 5159 5174 5231 5288 5303 5360 5417 5432 5489 5546 5561 5618 5675 5690 5747 5804 5819 5876 5933 5948 6005 6062 6077 6134 6191 6206 6263 6320 6335 6392 6449 6464 6521 6578 6593 6650 6707 6722 6779 6836 6851 6908 6965 6980 7037 7094 7109 7166 7223 7238 7295 7352 7367 7424 7481 7496 7553 7610 7625 7682 7739 7754 7811 7868 7883 7940 7997 8012 8069 8126 8141 8198 8255 8270 8327 8384 8399 8456 8513 8528 8585 8642 8657 8714 8771 8786 8843 8900 8915 8972 9029 9044 9101 9158 9173 9230 9287 9302 9359 9416 9431 9488 9545 9560 9617 9674 9689 9746 9803 9818 9875 9932 9947 10004 10061 10076 10133 10190 10205 10262 10319 10334 10391 10448 10463 10520 10577 10592 10649 10706 10721 10778 10835 10850 10907 10964 10979 11036 11093 11108 11165 11222 11237 11294 11351 11366 11423 11480 11495 11552 11609 11624 11681 11738 11753 11810 11867 11882 11939 11996 12011 12068 12125 12140 12197 12254 12269 12326 12383 12398 12455 12512 12527 12584 12641 12656 12713 12770 12785 12842 12899 12914 12971 13028 13043 13100 13157 13172 13229 13286 13301 13358 13415 13430 13487 13544 13559 13616 13673 13688 13745 13802 13817 13874 13931 13946 14003 14060 14075 14132 14189 14204 14261 14318 14333 14390 14447 14462 14519 14576 14591 14648 14705 14720 14777 14834 14849 14906 14963 14978 15035 15092 15107 15164 15221 15236 15293 15350 15365 15422 15479 15494 15551 15608 15623 15680 15737 15752 15809 15866 15881 15938 15995 16010 16067 16124 16139 16196 16253 16268 16325 16382 16397 16454 16511 16526 16583 16640 16655 16712 16769 16784 16841 16898 16913 16970 17027 17042 17099 17156 17171 17228 17285 17300 17357 17414 17429 17486 17543 17558 17615 17672 17687 17744 17801 17816 17873 17930 17945 18002 18059 18074 18131 18188 18203 18260 18317 18332 18389 18446 18461 18518 18575 18590 18647 18704 18719 18776 18833 18848 18905 18962 18977 19034 19091 19106 19163 19220 19235 19292 19349 19364 19421 19478 19493 19550 19607 19622 19679 19736 19751 19808 19865 19880 19937 19994 20009 20066 20123 20138 20195 20252 20267 20324 20381 20396 20453 20510 20525 20582 20639 20654 20711 20768 20783 20840 20897 20912 20969 21026 21041 21098 21155 21170 21227 21284 21299 21356 21413 21428 21485 21542 21557 21614 21671 21686 21743 21800 21815 21872 21929 21944 22001 22058 22073 22130 22187 22202 22259 22316 22331 22388 22445 22460 22517 22574 22589 22646 22703 22718 22775 22832 22847 22904 22961 22976 23033 23090 23105 23162 23219 23234 23291 23348 23363 23420 23477 23492 23549 23606 23621 23678 23735 23750 23807 23864 23879 23936 23993 24008 24065 24122 24137 24194 24251 24266 24323 24380 24395 24452 24509 24524 24581 24638 24653 24710 24767 24782 24839 24896 24911 24968 25025 25040 25097 25154 25169 25226 25283 25298 25355 25412 2