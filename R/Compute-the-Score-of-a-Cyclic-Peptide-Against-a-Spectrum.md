# Rosalind Problem: Compute the Score of a Cyclic Peptide Against a Spectrum

## Problem Understanding

We need to compute the score of a cyclic peptide against a given spectrum. The score is the number of matching masses between the theoretical spectrum of the cyclic peptide and the given experimental spectrum.

## Solution Approach

1. Generate the theoretical spectrum of a cyclic peptide
2. Compare it with the experimental spectrum
3. Count the matches

## R Implementation

```r
# Function to compute the score of a cyclic peptide against a spectrum
compute_score <- function(peptide, spectrum) {
  # Get theoretical spectrum of the cyclic peptide
  theoretical_spectrum <- get_cyclic_spectrum(peptide)
  
  # Convert spectrum to a list for easier comparison
  spectrum_list <- as.numeric(unlist(strsplit(spectrum, " ")))
  
  # Count matches
  score <- 0
  for (mass in theoretical_spectrum) {
    if (mass %in% spectrum_list) {
      score <- score + 1
      # Remove the matched mass to handle duplicates correctly
      spectrum_list <- spectrum_list[spectrum_list != mass]
    }
  }
  
  return(score)
}

# Function to generate cyclic spectrum of a peptide
get_cyclic_spectrum <- function(peptide) {
  # Amino acid masses
  masses <- c(
    "G" = 57, "A" = 71, "S" = 87, "P" = 97, "V" = 99,
    "T" = 101, "C" = 103, "I" = 113, "L" = 113, "N" = 114,
    "D" = 115, "K" = 128, "Q" = 128, "E" = 129, "M" = 131,
    "H" = 137, "F" = 147, "R" = 156, "Y" = 163, "W" = 186
  )
  
  # Convert peptide to masses
  peptide_masses <- sapply(strsplit(peptide, ""), function(aa) masses[aa])
  
  # Generate all subpeptides (including the full peptide)
  n <- length(peptide_masses)
  spectrum <- c(0)  # Start with 0 (empty subpeptide)
  
  # Generate all subpeptides for cyclic peptide
  for (i in 1:n) {
    for (j in 1:n) {
      if (i <= j) {
        # Linear subpeptide
        subpeptide_masses <- peptide_masses[i:j]
      } else {
        # Cyclic case - wrap around
        subpeptide_masses <- c(peptide_masses[i:n], peptide_masses[1:j])
      }
      spectrum <- c(spectrum, sum(subpeptide_masses))
    }
  }
  
  # Remove duplicate masses and sort
  spectrum <- sort(unique(spectrum))
  
  return(spectrum)
}

# Alternative cleaner implementation for cyclic spectrum
get_cyclic_spectrum_v2 <- function(peptide) {
  # Amino acid masses
  masses <- c(
    "G" = 57, "A" = 71, "S" = 87, "P" = 97, "V" = 99,
    "T" = 101, "C" = 103, "I" = 113, "L" = 113, "N" = 114,
    "D" = 115, "K" = 128, "Q" = 128, "E" = 129, "M" = 131,
    "H" = 137, "F" = 147, "R" = 156, "Y" = 163, "W" = 186
  )
  
  # Convert peptide to masses
  peptide_masses <- sapply(strsplit(peptide, ""), function(aa) masses[aa])
  
  # Create extended peptide for easier cyclic handling
  extended_peptide <- c(peptide_masses, peptide_masses)
  
  # Generate all subpeptides
  n <- length(peptide_masses)
  spectrum <- c(0)  # Start with 0 (empty subpeptide)
  
  # Generate all subpeptides of length 1 to n
  for (i in 1:n) {
    for (j in 1:n) {
      subpeptide_masses <- extended_peptide[i:(i+j-1)]
      spectrum <- c(spectrum, sum(subpeptide_masses))
    }
  }
  
  # Remove duplicate masses and sort
  spectrum <- sort(unique(spectrum))
  
  return(spectrum)
}

# Main function to solve the problem
solve_compute_score <- function(peptide, spectrum) {
  # Compute theoretical spectrum
  theoretical_spectrum <- get_cyclic_spectrum_v2(peptide)
  
  # Convert spectrum string to numeric vector
  experimental_spectrum <- as.numeric(unlist(strsplit(spectrum, " ")))
  
  # Count matches
  score <- 0
  for (mass in theoretical_spectrum) {
    # Find if this mass exists in experimental spectrum
    if (mass %in% experimental_spectrum) {
      score <- score + 1
      # Remove one occurrence to handle duplicates properly
      experimental_spectrum <- experimental_spectrum[experimental_spectrum != mass]
    }
  }
  
  return(score)
}

# Example usage:
# peptide <- "NQEL"
# spectrum <- "0 99 113 114 128 129 147 147 156 170 186 187 203 221 231 248 265 269 282 284 299 316 333 350 367 384 399 416 433 450 467 484 501 518 535 552 569 586 603 620 637 654 671 688 705 722 739 756 773 790 807 824 841 858 875 892 909 926 943 960 977 994 1011 1028 1045 1062 1079 1096 1113 1130 1147 1164 1181 1198 1215 1232 1249 1266 1283 1300 1317 1334 1351 1368 1385 1402 1419 1436 1453 1470 1487 1504 1521 1538 1555 1572 1589 1606 1623 1640 1657 1674 1691 1708 1725 1742 1759 1776 1793 1810 1827 1844 1861 1878 1895 1912 1929 1946 1963 1980 1997 2014 2031 2048 2065 2082 2099 2116 2133 2150 2167 2184 2201 2218 2235 2252 2269 2286 2303 2320 2337 2354 2371 2388 2405 2422 2439 2456 2473 2490 2507 2524 2541 2558 2575 2592 2609 2626 2643 2660 2677 2694 2711 2728 2745 2762 2779 2796 2813 2830 2847 2864 2881 2898 2915 2932 2949 2966 2983 3000 3017 3034 3051 3068 3085 3102 3119 3136 3153 3170 3187 3204 3221 3238 3255 3272 3289 3306 3323 3340 3357 3374 3391 3408 3425 3442 3459 3476 3493 3510 3527 3544 3561 3578 3595 3612 3629 3646 3663 3680 3697 3714 3731 3748 3765 3782 3799 3816 3833 3850 3867 3884 3901 3918 3935 3952 3969 3986 4003 4020 4037 4054 4071 4088 4105 4122 4139 4156 4173 4190 4207 4224 4241 4258 4275 4292 4309 4326 4343 4360 4377 4394 4411 4428 4445 4462 4479 4496 4513 4530 4547 4564 4581 4598 4615 4632 4649 4666 4683 4700 4717 4734 4751 4768 4785 4802 4819 4836 4853 4870 4887 4904 4921 4938 4955 4972 4989 5006 5023 5040 5057 5074 5091 5108 5125 5142 5159 5176 5193 5210 5227 5244 5261 5278 5295 5312 5329 5346 5363 5380 5397 5414 5431 5448 5465 5482 5499 5516 5533 5550 5567 5584 5601 5618 5635 5652 5669 5686 5703 5720 5737 5754 5771 5788 5805 5822 5839 5856 5873 5890 5907 5924 5941 5958 5975 5992 6009 6026 6043 6060 6077 6094 6111 6128 6145 6162 6179 6196 6213 6230 6247 6264 6281 6298 6315 6332 6349 6366 6383 6400 6417 6434 6451 6468 6485 6502 6519 6536 6553 6570 6587 6604 6621 6638 6655 6672 6689 6706 6723 6740 6757 6774 6791 6808 6825 6842 6859 6876 6893 6910 6927 6944 6961 6978 6995 7012 7029 7046 7063 7080 7097 7114 7131 7148 7165 7182 7199 7216 7233 7250 7267 7284 7301 7318 7335 7352 7369 7386 7403 7420 7437 7454 7471 7488 7505 7522 7539 7556 7573 7590 7607 7624 7641 7658 7675 7692 7709 7726 7743 7760 7777 7794 7811 7828 7845 7862 7879 7896 7913 7930 7947 7964 7981 7998 8015 8032 8049 8066 8083 8100 8117 8134 8151 8168 8185 8202 8219 8236 8253 8270 8287 8304 8321 8338 8355 8372 8389 8406 8423 8440 8457 8474 8491 8508 8525 8542 8559 8576 8593 8610 8627 8644 8661 8678 8695 8712 8729 8746 8763 8780 8797 8814 8831 8848 8865 8882 8899 8916 8933 8950 8967 8984 9001 9018 9035 9052 9069 9086 9103 9120 9137 9154 9171 9188 9205 9222 9239 9256 9273 9290 9307 9324 9341 9358 9375 9392 9409 9426 9443 9460 9477 9494 9511 9528 9545 9562 9579 9596 9613 9630 9647 9664 9681 9698 9715 9732 9749 9766 9783 9800 9817 9834 9851 9868 9885 9902 9919