```kotlin
import java.io.File

fun main() {
    val input = File("rosalind_prot.txt").readText().trim()
    val result = translateRNAtoProtein(input)
    println(result)
}

fun translateRNAtoProtein(rna: String): String {
    val codonTable = mapOf(
        "UUU" to 'F', "UUC" to 'F', "UUA" to 'L', "UUG" to 'L',
        "CUU" to 'L', "CUC" to 'L', "CUA" to 'L', "CUG" to 'L',
        "AUU" to 'I', "AUC" to 'I', "AUA" to 'I', "GUU" to 'V',
        "GUC" to 'V', "GUA" to 'V', "GUG" to 'V',
        "UCU" to 'S', "UCC" to 'S', "UCA" to 'S', "UCG" to 'S',
        "CCU" to 'P', "CCC" to 'P', "CCA" to 'P', "CCG" to 'P',
        "ACU" to 'T', "ACC" to 'T', "ACA" to 'T', "ACG" to 'T',
        "GCU" to 'A', "GCC" to 'A', "GCA" to 'A', "GCG" to 'A',
        "UAU" to 'Y', "UAC" to 'Y',
        "CAU" to 'H', "CAC" to 'H',
        "AAU" to 'N', "AAC" to 'N',
        "GAU" to 'D', "GAC" to 'D',
        "UGU" to 'C', "UGC" to 'C',
        "CAA" to 'Q', "CAG" to 'Q',
        "AAU" to 'N', "AAC" to 'N',
        "GAU" to 'D', "GAC" to 'D',
        "AGU" to 'S', "AGC" to 'S',
        "AGA" to 'R', "AGG" to 'R',
        "CGU" to 'R', "CGC" to 'R', "CGA" to 'R', "CGG" to 'R',
        "GGU" to 'G', "GGC" to 'G', "GGA" to 'G', "GGG" to 'G',
        "UGG" to 'W',
        "UAA" to '*', "UAG" to '*', "UGA" to '*'
    )
    
    val protein = StringBuilder()
    
    for (i in 0 until rna.length step 3) {
        if (i + 2 < rna.length) {
            val codon = rna.substring(i, i + 3)
            val aminoAcid = codonTable[codon]
            if (aminoAcid == '*') break
            if (aminoAcid != null) {
                protein.append(aminoAcid)
            }
        }
    }
    
    return protein.toString()
}
```

