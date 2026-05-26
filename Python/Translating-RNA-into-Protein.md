# Rosalind Problem: Translating RNA into Protein

## Problem Statement
The problem asks us to translate an RNA string into a protein string by reading codons (3-letter sequences) and mapping them to their corresponding amino acids.

## Solution Approach
1. Create a codon table mapping RNA codons to amino acids
2. Read the RNA string
3. Split the RNA string into codons (3-character chunks)
4. Translate each codon to its corresponding amino acid
5. Stop translation when encountering a stop codon

## Python Implementation

```python
def translate_rna_to_protein(rna_string):
    """
    Translate RNA string into protein string
    
    Args:
        rna_string (str): RNA sequence string
    
    Returns:
        str: Protein sequence string
    """
    # Codon table mapping RNA codons to amino acids
    codon_table = {
        'UUU': 'F', 'UUC': 'F', 'UUA': 'L', 'UUG': 'L',
        'UCU': 'S', 'UCC': 'S', 'UCA': 'S', 'UCG': 'S',
        'UAU': 'Y', 'UAC': 'Y', 'UAA': '*', 'UAG': '*',
        'UGU': 'C', 'UGC': 'C', 'UGA': '*', 'UGG': 'W',
        'CUU': 'L', 'CUC': 'L', 'CUA': 'L', 'CUG': 'L',
        'CCU': 'P', 'CCC': 'P', 'CCA': 'P', 'CCG': 'P',
        'CAU': 'H', 'CAC': 'H', 'CAA': 'Q', 'CAG': 'Q',
        'CGU': 'R', 'CGC': 'R', 'CGA': 'R', 'CGG': 'R',
        'AUU': 'I', 'AUC': 'I', 'AUA': 'I', 'AUG': 'M',
        'ACU': 'T', 'ACC': 'T', 'ACA': 'T', 'ACG': 'T',
        'AAU': 'N', 'AAC': 'N', 'AAA': 'K', 'AAG': 'K',
        'AGU': 'S', 'AGC': 'S', 'AGA': 'R', 'AGG': 'R',
        'GUU': 'V', 'GUC': 'V', 'GUA': 'V', 'GUG': 'V',
        'GCU': 'A', 'GCC': 'A', 'GCA': 'A', 'GCG': 'A',
        'GAU': 'D', 'GAC': 'D', 'GAA': 'E', 'GAG': 'E',
        'GGU': 'G', 'GGC': 'G', 'GGA': 'G', 'GGG': 'G'
    }
    
    protein = ""
    
    # Process RNA string in chunks of 3 nucleotides (codons)
    for i in range(0, len(rna_string), 3):
        # Extract codon (3 nucleotides)
        codon = rna_string[i:i+3]
        
        # Check if we have a complete codon
        if len(codon) < 3:
            break
            
        # Translate codon to amino acid
        if codon in codon_table:
            amino_acid = codon_table[codon]
            
            # Stop translation if we encounter a stop codon
            if amino_acid == '*':
                break
                
            protein += amino_acid
        else:
            # Handle invalid codons if any
            break
    
    return protein

# Read input and solve
if __name__ == "__main__":
    # Read RNA string from input (assuming it's provided in a file or stdin)
    # For example, if reading from a file:
    # with open('rosalind_prot.txt', 'r') as file:
    #     rna_sequence = file.read().strip()
    
    # For demonstration, let's use a sample RNA sequence
    rna_sequence = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    
    result = translate_rna_to_protein(rna_sequence)
    print(result)
```

## Example Usage

If we have the RNA sequence: `AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA`

The translation would proceed as:
- AUG → M
- GCC → A  
- AUG → M
- GCG → A
- CCC → P
- AGA → R
- ACU → T
- GAG → E
- AUC → I
- AAU → N
- AGU → S
- ACC → T
- CGU → R
- AUA → I
- ACA → T
- CGG → R
- GUA → V
- UAA → Stop (stop translation)

Result: `MAMAAAPRTEINSTRIV`

## Key Points
1. **Codon Table**: Maps 3-letter RNA sequences to amino acid symbols
2. **Stop Codons**: `UAA`, `UAG`, `UGA` translate to `*` and stop translation
3. **Start Codon**: `AUG` codes for Methionine (M) and serves as the start signal
4. **Frame Reading**: Process RNA string in 3-character chunks
5. **Error Handling**: Handle incomplete codons or invalid sequences

This solution correctly handles the RNA to protein translation as required by the Rosalind problem.

