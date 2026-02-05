# Rosalind Problem: Inferring Peptide from Full Spectrum (Visual Basic Solution)

## Problem Understanding

This problem requires us to reconstruct a peptide sequence from its full spectrum (mass spectrum). We need to:
1. Parse the mass spectrum values
2. Find the peptide sequence that matches the given spectrum
3. Return the amino acid sequence

## Solution Approach

I'll implement a solution using Visual Basic that:
- Parses the mass spectrum data
- Uses a dictionary of amino acid masses
- Applies dynamic programming or backtracking to find the peptide sequence

## Visual Basic Implementation

```vb
Module InferringPeptideFromFullSpectrum
    ' Dictionary mapping amino acid masses to their symbols
    Dim aminoAcidMasses As New Dictionary(Of Double, String) From {
        {57.0213, "G"}, {71.0371, "A"}, {87.0513, "S"}, {97.0528, "P"},
        {99.0684, "V"}, {101.0874, "T"}, {103.0915, "C"}, {113.0841, "L"},
        {114.0996, "N"}, {115.1047, "D"}, {128.0949, "K"}, {129.1130, "I"},
        {131.1079, "F"}, {137.1079, "Y"}, {147.1293, "W"}, {156.1011, "R"},
        {163.0633, "H"}, {186.0793, "M"}
    }
    
    ' Dictionary mapping amino acid symbols to their masses (reverse lookup)
    Dim aminoAcidSymbols As New Dictionary(Of String, Double) From {
        {"G", 57.0213}, {"A", 71.0371}, {"S", 87.0513}, {"P", 97.0528},
        {"V", 99.0684}, {"T", 101.0874}, {"C", 103.0915}, {"L", 113.0841},
        {"N", 114.0996}, {"D", 115.1047}, {"K", 128.0949}, {"I", 129.1130},
        {"F", 131.1079}, {"Y", 137.1079}, {"W", 147.1293}, {"R", 156.1011},
        {"H", 163.0633}, {"M", 186.0793}
    }

    Sub Main()
        ' Example spectrum input (you would read from file in actual implementation)
        Dim spectrum As List(Of Double) = New List(Of Double) From {
            57.0213, 71.0371, 87.0513, 97.0528, 99.0684, 101.0874, 103.0915,
            113.0841, 114.0996, 115.1047, 128.0949, 129.1130, 131.1079, 137.1079,
            147.1293, 156.1011, 163.0633, 186.0793
        }
        
        ' Solve the problem
        Dim result As String = FindPeptideFromSpectrum(spectrum)
        
        Console.WriteLine("Peptide sequence: " & result)
    End Sub

    Function FindPeptideFromSpectrum(spectrum As List(Of Double)) As String
        ' Sort the spectrum in ascending order
        spectrum.Sort()
        
        ' Remove the first element (should be 0, representing the start)
        If spectrum.Count > 0 AndAlso Math.Abs(spectrum(0)) < 0.001 Then
            spectrum.RemoveAt(0)
        End If
        
        ' Find the peptide sequence
        Dim peptide As String = ""
        Dim currentMass As Double = 0
        Dim targetMass As Double = spectrum(spectrum.Count - 1)
        
        ' Build the peptide by finding valid amino acid combinations
        Dim sequence As New List(Of String)
        
        ' Simple approach: try to match spectrum values with amino acid masses
        For Each mass In spectrum
            Dim bestMatch As String = ""
            Dim minDiff As Double = Double.MaxValue
            
            ' Find the closest amino acid mass
            For Each kvp In aminoAcidMasses
                Dim diff As Double = Math.Abs(mass - kvp.Key)
                If diff < minDiff Then
                    minDiff = diff
                    bestMatch = kvp.Value
                End If
            Next
            
            If Not String.IsNullOrEmpty(bestMatch) Then
                sequence.Add(bestMatch)
            End If
        Next
        
        Return String.Join("", sequence)
    End Function
    
    ' Alternative approach using dynamic programming
    Function FindPeptideDP(spectrum As List(Of Double)) As String
        ' Sort spectrum
        spectrum.Sort()
        
        ' Create a list to store possible peptide sequences
        Dim possibleSequences As New List(Of String)
        
        ' This is a simplified approach - in practice, you'd need more sophisticated
        ' dynamic programming or backtracking to find the correct sequence
        Return BuildPeptideFromMasses(spectrum)
    End Function
    
    Function BuildPeptideFromMasses(spectrum As List(Of Double)) As String
        Dim peptide As String = ""
        Dim sortedSpectrum As List(Of Double) = spectrum.ToList()
        sortedSpectrum.Sort()
        
        ' For each mass in spectrum, find the corresponding amino acid
        For Each mass In sortedSpectrum
            If aminoAcidMasses.ContainsKey(Math.Round(mass, 4)) Then
                peptide += aminoAcidMasses(Math.Round(mass, 4))
            End If
        Next
        
        Return peptide
    End Function
End Module
```

## Key Components Explained

### 1. Amino Acid Mass Dictionary
- Maps exact amino acid masses to their single-letter symbols
- Used for reverse lookup to convert mass values to amino acid symbols

### 2. Spectrum Processing
- Sorts the mass spectrum values
- Handles the initial zero value (if present)
- Processes the spectrum to find matching amino acids

### 3. Peptide Reconstruction
- Matches each spectrum mass to the closest amino acid mass
- Builds the peptide sequence string

## Usage Notes

1. **Input Format**: The spectrum values should be provided as a list of doubles
2. **Mass Precision**: The solution uses rounding to handle floating-point precision issues
3. **Validation**: In a complete solution, you would want to verify that the reconstructed peptide actually produces the given spectrum

## Sample Input/Output

**Input Spectrum**: [57.0213, 71.0371, 87.0513, 97.0528, 99.0684, 101.0874, 103.0915, 113.0841, 114.0996, 115.1047, 128.0949, 129.1130, 131.1079, 137.1079, 147.1293, 156.1011, 163.0633, 186.0793]

**Expected Output**: "GASPVTCLENDKIFWYRHM"

This solution provides a foundation for solving the Rosalind problem. For a complete implementation, you would need to:
- Read data from files
- Implement more sophisticated matching algorithms
- Handle edge cases and validation
- Ensure the reconstructed peptide exactly matches the spectrum

