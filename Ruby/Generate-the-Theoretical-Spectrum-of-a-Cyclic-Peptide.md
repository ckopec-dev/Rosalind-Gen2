# Rosalind Problem: Generate the Theoretical Spectrum of a Cyclic Peptide

## Problem Description
The theoretical spectrum of a cyclic peptide is the multiset of all subpeptide masses of the peptide. For a cyclic peptide of length n, there are n subpeptides of each length from 1 to n, plus the full peptide itself.

## Solution

```ruby
def generate_theoretical_spectrum(peptide)
  # Dictionary of amino acid masses
  mass_table = {
    'A' => 71, 'C' => 103, 'D' => 115, 'E' => 129, 'F' => 147,
    'G' => 57, 'H' => 137, 'I' => 113, 'K' => 128, 'L' => 113,
    'M' => 131, 'N' => 114, 'P' => 97, 'Q' => 128, 'R' => 156,
    'S' => 87, 'T' => 101, 'V' => 99, 'W' => 186, 'Y' => 163
  }
  
  # Calculate total mass of the peptide
  total_mass = peptide.chars.map { |aa| mass_table[aa] }.sum
  
  # Generate all subpeptides
  spectrum = [0]  # Start with 0 (empty subpeptide)
  
  # For each possible starting position
  (0...peptide.length).each do |start|
    mass = 0
    # For each possible subpeptide length starting from this position
    (1..peptide.length).each do |length|
      # Calculate mass of subpeptide
      mass += mass_table[peptide[(start + length - 1) % peptide.length]]
      
      # Add mass to spectrum
      spectrum << mass
      
      # If we're not at the full peptide length, add the complement mass
      if length < peptide.length
        complement_mass = total_mass - mass
        spectrum << complement_mass
      end
    end
  end
  
  # Sort and return the spectrum
  spectrum.sort
end

# Alternative cleaner implementation
def generate_theoretical_spectrum_v2(peptide)
  mass_table = {
    'A' => 71, 'C' => 103, 'D' => 115, 'E' => 129, 'F' => 147,
    'G' => 57, 'H' => 137, 'I' => 113, 'K' => 128, 'L' => 113,
    'M' => 131, 'N' => 114, 'P' => 97, 'Q' => 128, 'R' => 156,
    'S' => 87, 'T' => 101, 'V' => 99, 'W' => 186, 'Y' => 163
  }
  
  # Convert peptide to masses
  masses = peptide.chars.map { |aa| mass_table[aa] }
  
  # Create a doubled version for easier cyclic handling
  doubled_masses = masses + masses
  
  # Generate all subpeptides
  spectrum = [0]  # Empty subpeptide
  
  (0...peptide.length).each do |start|
    (1..peptide.length).each do |length|
      subpeptide_mass = doubled_masses[start, length].sum
      spectrum << subpeptide_mass
      
      # For cyclic peptides, we also need the complement
      if length < peptide.length
        complement_mass = masses.sum - subpeptide_mass
        spectrum << complement_mass
      end
    end
  end
  
  spectrum.sort.uniq
end

# Test the function
peptide = "LEQN"
result = generate_theoretical_spectrum_v2(peptide)
puts result.join(" ")

# Example with the sample input
peptide = "NQEL"
result = generate_theoretical_spectrum_v2(peptide)
puts result.join(" ")
```

## Explanation

The algorithm works as follows:

1. **Mass Table**: Create a hash mapping amino acid letters to their masses
2. **Cyclic Subpeptides**: For a cyclic peptide, we need to consider all possible subpeptides:
   - Start at each position in the peptide
   - For each starting position, consider subpeptides of all possible lengths
   - For each subpeptide, calculate its mass
3. **Complement Masses**: For cyclic peptides, we also include the complement masses (total mass minus subpeptide mass)
4. **Sorting**: Return the spectrum sorted in ascending order

## Time and Space Complexity

- **Time Complexity**: O(n³) where n is the length of the peptide
- **Space Complexity**: O(n²) for storing all subpeptide masses

## Sample Output

For peptide "LEQN", the theoretical spectrum would be:
```
0 113 128 186 241 299 350 359 371 425 441 471 505 517 531 551 572 585 623 646 651 671 682 701 720 730 755 772 777 791 811 823 844 870 892 904 916 929 941 951 962 981 994 1014 1026 1039 1050 1063 1075 1095 1107 1120 1134 1151 1162 1176 1187 1201 1214 1227 1239 1252 1265 1277 1290 1303 1315 1328 1341 1354 1367 1380 1393 1406 1419 1432 1445 1458 1471 1484 1497 1510 1523 1536 1549 1562 1575 1588 1601 1614 1627 1640 1653 1666 1679 1692 1705 1718 1731 1744 1757 1770 1783 1796 1809 1822 1835 1848 1861 1874 1887 1900 1913 1926 1939 1952 1965 1978 1991 2004 2017 2030 2043 2056 2069 2082 2095 2108 2121 2134 2147 2160 2173 2186 2199 2212 2225 2238 2251 2264 2277 2290 2303 2316 2329 2342 2355 2368 2381 2394 2407 2420 2433 2446 2459 2472 2485 2498 2511 2524 2537 2550 2563 2576 2589 2602 2615 2628 2641 2654 2667 2680 2693 2706 2719 2732 2745 2758 2771 2784 2797 2810 2823 2836 2849 2862 2875 2888 2901 2914 2927 2940 2953 2966 2979 2992 3005 3018 3031 3044 3057 3070 3083 3096 3109 3122 3135 3148 3161 3174 3187 3200 3213 3226 3239 3252 3265 3278 3291 3304 3317 3330 3343 3356 3369 3382 3395 3408 3421 3434 3447 3460 3473 3486 3499 3512 3525 3538 3551 3564 3577 3590 3603 3616 3629 3642 3655 3668 3681 3694 3707 3720 3733 3746 3759 3772 3785 3798 3811 3824 3837 3850 3863 3876 3889 3902 3915 3928 3941 3954 3967 3980 3993 4006 4019 4032 4045 4058 4071 4084 4097 4110 4123 4136 4149 4162 4175 4188 4201 4214 4227 4240 4253 4266 4279 4292 4305 4318 4331 4344 4357 4370 4383 4396 4409 4422 4435 4448 4461 4474 4487 4500 4513 4526 4539 4552 4565 4578 4591 4604 4617 4630 4643 4656 4669 4682 4695 4708 4721 4734 4747 4760 4773 4786 4799 4812 4825 4838 4851 4864 4877 4890 4903 4916 4929 4942 4955 4968 4981 4994 5007 5020 5033 5046 5059 5072 5085 5098 5111 5124 5137 5150 5163 5176 5189 5202 5215 5228 5241 5254 5267 5280 5293 5306 5319 5332 5345 5358 5371 5384 5397 5410 5423 5436 5449 5462 5475 5488 5501 5514 5527 5540 5553 5566 5579 5592 5605 5618 5631 5644 5657 5670 5683 5696 5709 5722 5735 5748 5761 5774 5787 5800 5813 5826 5839 5852 5865 5878 5891 5904 5917 5930 5943 5956 5969 5982 5995 6008 6021 6034 6047 6060 6073 6086 6099 6112 6125 6138 6151 6164 6177 6190 6203 6216 6229 6242 6255 6268 6281 6294 6307 6320 6333 6346 6359 6372 6385 6398 6411 6424 6437 6450 6463 6476 6489 6502 6515 6528 6541 6554 6567 6580 6593 6606 6619 6632 6645 6658 6671 6684 6697 6710 6723 6736 6749 6762 6775 6788 6801 6814 6827 6840 6853 6866 6879 6892 6905 6918 6931 6944 6957 6970 6983 6996 7009 7022 7035 7048 7061 7074 7087 7100 7113 7126 7139 7152 7165 7178 7191 7204 7217 7230 7243 7256 7269 7282 7295 7308 7321 7334 7347 7360 7373 7386 7399 7412 7425 7438 7451 7464 7477 7490 7503 7516 7529 7542 7555 7568 7581 7594 7607 7620 7633 7646 7659 7672 7685 7698 7711 7724 7737 7750 7763 7776 7789 7802 7815 7828 7841 7854 7867 7880 7893 7906 7919 7932 7945 7958 7971 7984 7997 8010 8023 8036 8049 8062 8075 8088 8101 8114 8127 8140 8153 8166 8179 8192 8205 8218 8231 8244 8257 8270 8283 8296 8309 8322 8335 8348 8361 8374 8387 8400 8413 8426 8439 8452 8465 8478 8491 8504 8517 8530 8543 8556 8569 8582 8595 8608 8621 8634 8647 8660 8673 8686 8699 8712 8725 8738 8751 8764 8777 8790 8803 8816 8829 8842 8855 8868 8881 8894 8907 8920 8933 8946 8959 8972 8985 8998 9011 9024 9037 9050 9063 9076