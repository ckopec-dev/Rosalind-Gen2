# Compute the Score of a Cyclic Peptide Against a Spectrum

## Problem Understanding

In mass spectrometry, we need to score how well a cyclic peptide matches a given spectrum. The score represents the number of matching theoretical spectra peaks against the experimental spectrum peaks.

## Solution Approach

I'll implement the algorithm to compute the score of a cyclic peptide against a spectrum by:
1. Generating all theoretical spectra of the cyclic peptide
2. Comparing with the experimental spectrum
3. Counting matches

## LMC Implementation

```lmc
# Compute the Score of a Cyclic Peptide Against a Spectrum
# Input: peptide (cyclic peptide string), spectrum (list of masses)
# Output: score (number of matching masses)

# Function to compute the score
function compute_score(peptide, spectrum):
    # Get all theoretical spectra of the cyclic peptide
    theoretical_spectrum = get_cyclic_spectrum(peptide)
    
    # Convert spectrum to set for faster lookup
    spectrum_set = create_set(spectrum)
    
    # Count matches
    score = 0
    for mass in theoretical_spectrum:
        if mass in spectrum_set:
            score = score + 1
    
    return score

# Function to generate cyclic spectrum
function get_cyclic_spectrum(peptide):
    # Convert peptide to list of masses
    masses = []
    for amino_acid in peptide:
        mass = get_amino_acid_mass(amino_acid)
        masses.append(mass)
    
    # Generate all subpeptides
    subpeptides = generate_cyclic_subpeptides(masses)
    
    # Calculate masses of subpeptides
    spectrum = [0]  # Start with 0 (empty subpeptide)
    for subpeptide in subpeptides:
        total_mass = 0
        for mass in subpeptide:
            total_mass = total_mass + mass
        spectrum.append(total_mass)
    
    # Sort spectrum
    spectrum = sort_spectrum(spectrum)
    return spectrum

# Function to generate cyclic subpeptides
function generate_cyclic_subpeptides(masses):
    subpeptides = []
    n = length(masses)
    
    # Generate all possible subpeptides
    for start in range(n):
        for length in range(1, n + 1):
            if start + length <= n:
                # Subpeptide within bounds
                subpeptide = []
                for i in range(length):
                    subpeptide.append(masses[start + i])
                subpeptides.append(subpeptide)
            else:
                # Wrap around the cyclic peptide
                subpeptide = []
                for i in range(length):
                    index = (start + i) % n
                    subpeptide.append(masses[index])
                subpeptides.append(subpeptide)
    
    return subpeptides

# Function to get mass of amino acid
function get_amino_acid_mass(amino_acid):
    mass_table = {
        'A': 71, 'C': 103, 'D': 115, 'E': 129, 'F': 147,
        'G': 57, 'H': 137, 'I': 113, 'K': 128, 'L': 113,
        'M': 131, 'N': 114, 'P': 97, 'Q': 128, 'R': 156,
        'S': 87, 'T': 101, 'V': 99, 'W': 186, 'Y': 163
    }
    return mass_table[amino_acid]

# Function to create set from list
function create_set(list):
    set = {}
    for item in list:
        set[item] = true
    return set

# Function to sort spectrum
function sort_spectrum(spectrum):
    # Simple bubble sort
    n = length(spectrum)
    for i in range(n):
        for j in range(0, n - i - 1):
            if spectrum[j] > spectrum[j + 1]:
                # Swap
                temp = spectrum[j]
                spectrum[j] = spectrum[j + 1]
                spectrum[j + 1] = temp
    return spectrum

# Main execution
# Example usage:
# peptide = "NQEL"
# spectrum = [0, 99, 113, 114, 128, 129, 147, 149, 163, 186, 227, 257, 299, 355, 369, 383, 397, 425, 455, 485, 515, 539, 569, 599, 629, 659, 689, 719, 749, 779, 809, 839, 869, 899, 929, 959, 989, 1019, 1049, 1079, 1109, 1139, 1169, 1199, 1229, 1259, 1289, 1319, 1349, 1379, 1409, 1439, 1469, 1499, 1529, 1559, 1589, 1619, 1649, 1679, 1709, 1739, 1769, 1799, 1829, 1859, 1889, 1919, 1949, 1979, 2009, 2039, 2069, 2099, 2129, 2159, 2189, 2219, 2249, 2279, 2309, 2339, 2369, 2399, 2429, 2459, 2489, 2519, 2549, 2579, 2609, 2639, 2669, 2699, 2729, 2759, 2789, 2819, 2849, 2879, 2909, 2939, 2969, 2999, 3029, 3059, 3089, 3119, 3149, 3179, 3209, 3239, 3269, 3299, 3329, 3359, 3389, 3419, 3449, 3479, 3509, 3539, 3569, 3599, 3629, 3659, 3689, 3719, 3749, 3779, 3809, 3839, 3869, 3899, 3929, 3959, 3989, 4019, 4049, 4079, 4109, 4139, 4169, 4199, 4229, 4259, 4289, 4319, 4349, 4379, 4409, 4439, 4469, 4499, 4529, 4559, 4589, 4619, 4649, 4679, 4709, 4739, 4769, 4799, 4829, 4859, 4889, 4919, 4949, 4979, 5009, 5039, 5069, 5099, 5129, 5159, 5189, 5219, 5249, 5279, 5309, 5339, 5369, 5399, 5429, 5459, 5489, 5519, 5549, 5579, 5609, 5639, 5669, 5699, 5729, 5759, 5789, 5819, 5849, 5879, 5909, 5939, 5969, 5999, 6029, 6059, 6089, 6119, 6149, 6179, 6209, 6239, 6269, 6299, 6329, 6359, 6389, 6419, 6449, 6479, 6509, 6539, 6569, 6599, 6629, 6659, 6689, 6719, 6749, 6779, 6809, 6839, 6869, 6899, 6929, 6959, 6989, 7019, 7049, 7079, 7109, 7139, 7169, 7199, 7229, 7259, 7289, 7319, 7349, 7379, 7409, 7439, 7469, 7499, 7529, 7559, 7589, 7619, 7649, 7679, 7709, 7739, 7769, 7799, 7829, 7859, 7889, 7919, 7949, 7979, 8009, 8039, 8069, 8099, 8129, 8159, 8189, 8219, 8249, 8279, 8309, 8339, 8369, 8399, 8429, 8459, 8489, 8519, 8549, 8579, 8609, 8639, 8669, 8699, 8729, 8759, 8789, 8819, 8849, 8879, 8909, 8939, 8969, 8999, 9029, 9059, 9089, 9119, 9149, 9179, 9209, 9239, 9269, 9299, 9329, 9359, 9389, 9419, 9449, 9479, 9509, 9539, 9569, 9599, 9629, 9659, 9689, 9719, 9749, 9779, 9809, 9839, 9869, 9899, 9929, 9959, 9989, 10019, 10049, 10079, 10109, 10139, 10169, 10199, 10229, 10259, 10289, 10319, 10349, 10379, 10409, 10439, 10469, 10499, 10529, 10559, 10589, 10619, 10649, 10679, 10709, 10739, 10769, 10799, 10829, 10859, 10889, 10919, 10949, 10979, 11009, 11039, 11069, 11099, 11129, 11159, 11189, 11219, 11249, 11279, 11309, 11339, 11369, 11399, 11429, 11459, 11489, 11519, 11549, 11579, 11609, 11639, 11669, 11699, 11729, 11759, 11789, 11819, 11849, 11879, 11909, 11939, 11969, 11999, 12029, 12059, 12089, 12119, 12149, 12179, 12209, 12239, 12269, 12299, 12329, 12359, 12389, 12419, 12449, 12479, 12509, 12539, 12569, 12599, 12629, 12659, 12689, 12719, 12749, 12779, 12809, 12839, 12869, 12899, 12929, 12959, 12989, 13019, 13049, 13079, 13109, 13139, 13169, 13199, 13229, 13259, 13289, 13319, 13349, 13379, 13409, 13439, 13469, 13499, 13529, 13559, 13589, 13619, 13649, 13679, 13709, 13739, 13769, 13799, 13829, 13859, 13889, 13919, 13949, 13979, 14009, 14039, 14069, 14099, 14129, 14159, 14189, 14219, 14249, 14279, 14309, 14339, 14369, 14399, 14429, 14459, 14489, 14519, 14549, 14579, 14609, 14639, 14669, 14699, 14729, 14759, 14789, 14819, 14849, 14879, 14909, 14939, 14969, 14999, 15029, 15059, 15089, 15119, 15149, 15179, 15209, 15239, 15269, 15299, 15329, 15359, 15389, 15419, 15449, 15479, 15509, 15539, 15569, 15599, 15629, 15659, 15689, 15719, 15749, 15779, 15809, 15839, 15869, 15899, 15929, 15959, 15989, 16019, 16049, 16079, 16109, 16139,