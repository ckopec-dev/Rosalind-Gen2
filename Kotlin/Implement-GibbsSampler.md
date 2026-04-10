```kotlin
import kotlin.math.*

fun main() {
    val lines = readLines()
    
    val (k, t, N) = lines[0].split(" ").map { it.toInt() }
    val dna = lines.drop(1).take(t)
    
    val bestMotifs = gibbsSampler(dna, k, t, N)
    println(bestMotifs.joinToString("\n"))
}

fun gibbsSampler(dna: List<String>, k: Int, t: Int, N: Int): List<String> {
    // Initialize random motifs
    val motifs = mutableListOf<String>()
    for (i in 0 until t) {
        val randomStart = (0 until dna[i].length - k + 1).random()
        motifs.add(dna[i].substring(randomStart, randomStart + k))
    }
    
    var bestMotifs = motifs.toList()
    var bestScore = calculateScore(bestMotifs)
    
    for (iter in 0 until N) {
        val i = (0 until t).random()
        val motifsWithoutI = motifs.filterIndexed { index, _ -> index != i }
        
        // Create profile from motifs without i
        val profile = createProfile(motifsWithoutI)
        
        // Generate new motif for sequence i
        val newMotif = generateMotif(dna[i], profile, k)
        motifs[i] = newMotif
        
        // Update best motifs if current is better
        val currentScore = calculateScore(motifs)
        if (currentScore < bestScore) {
            bestScore = currentScore
            bestMotifs = motifs.toList()
        }
    }
    
    return bestMotifs
}

fun createProfile(motifs: List<String>): List<Map<Char, Double>> {
    val k = motifs[0].length
    val profile = mutableListOf<Map<Char, Double>>()
    
    for (i in 0 until k) {
        val counts = mutableMapOf<Char, Int>()
        for (motif in motifs) {
            val nucleotide = motif[i]
            counts[nucleotide] = counts.getOrDefault(nucleotide, 0) + 1
        }
        
        val row = mutableMapOf<Char, Double>()
        for (nucleotide in listOf('A', 'C', 'G', 'T')) {
            row[nucleotide] = (counts.getOrDefault(nucleotide, 0) + 1.0) / (motifs.size + 4.0)
        }
        profile.add(row)
    }
    
    return profile
}

fun generateMotif(sequence: String, profile: List<Map<Char, Double>>, k: Int): String {
    val probabilities = mutableListOf<Double>()
    
    for (i in 0 until sequence.length - k + 1) {
        val kmer = sequence.substring(i, i + k)
        var probability = 1.0
        
        for (j in kmer.indices) {
            probability *= profile[j][kmer[j]]!!
        }
        
        probabilities.add(probability)
    }
    
    // Normalize probabilities
    val sum = probabilities.sum()
    val normalized = probabilities.map { it / sum }
    
    // Select index based on probabilities
    val random = kotlin.random.Random.nextDouble()
    var cumulative = 0.0
    for (i in normalized.indices) {
        cumulative += normalized[i]
        if (random <= cumulative) {
            return sequence.substring(i, i + k)
        }
    }
    
    return sequence.substring(0, k)
}

fun calculateScore(motifs: List<String>): Int {
    val k = motifs[0].length
    var score = 0
    
    for (i in 0 until k) {
        val nucleotides = motifs.map { it[i] }
        val counts = nucleotides.groupingBy { it }.eachCount()
        val maxCount = counts.values.maxOrNull() ?: 0
        score += motifs.size - maxCount
    }
    
    return score
}

fun readLines(): List<String> {
    val lines = mutableListOf<String>()
    while (true) {
        val line = readLine() ?: break
        lines.add(line)
    }
    return lines
}
```

