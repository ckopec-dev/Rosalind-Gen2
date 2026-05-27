# Rosalind Problem: Isolating Symbols in Alignments (Scala Solution)

## Problem Understanding

The problem asks us to find the symbols that appear in at least one of the sequences in a multiple sequence alignment, and count how many times each symbol appears across all sequences.

## Solution Approach

1. Parse the input FASTA format sequences
2. Extract all unique symbols from the sequences
3. Count occurrences of each symbol across all sequences
4. Return the symbols in alphabetical order with their counts

## Scala Implementation

```scala
import scala.io.Source
import scala.collection.mutable

object IsolatingSymbolsInAlignments {
  
  def main(args: Array[String]): Unit = {
    // Read input from file or stdin
    val input = Source.fromFile("input.txt").getLines().toList
    val sequences = parseFasta(input)
    
    // Find and count symbols
    val symbolCounts = countSymbols(sequences)
    
    // Print results in alphabetical order
    symbolCounts.toList.sortBy(_._1).foreach { case (symbol, count) =>
      println(s"$symbol $count")
    }
  }
  
  def parseFasta(lines: List[String]): List[String] = {
    val sequenceLines = mutable.ListBuffer[String]()
    val sequences = mutable.ListBuffer[String]()
    
    lines.foreach { line =>
      if (line.startsWith(">")) {
        if (sequenceLines.nonEmpty) {
          sequences += sequenceLines.mkString
          sequenceLines.clear()
        }
      } else {
        sequenceLines += line
      }
    }
    
    // Add the last sequence
    if (sequenceLines.nonEmpty) {
      sequences += sequenceLines.mkString
    }
    
    sequences.toList
  }
  
  def countSymbols(sequences: List[String]): Map[Char, Int] = {
    val symbolCount = mutable.Map[Char, Int]()
    
    sequences.foreach { sequence =>
      sequence.foreach { symbol =>
        symbolCount(symbol) = symbolCount.getOrElse(symbol, 0) + 1
      }
    }
    
    symbolCount.toMap
  }
}
```

## Alternative Compact Solution

```scala
import scala.io.Source

object IsolatingSymbolsInAlignments {
  
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toList
    val sequences = parseFasta(input)
    
    val symbolCounts = sequences
      .flatMap(_.toList)
      .groupBy(identity)
      .mapValues(_.length)
      .toList
      .sortBy(_._1)
    
    symbolCounts.foreach { case (symbol, count) =>
      println(s"$symbol $count")
    }
  }
  
  def parseFasta(lines: List[String]): List[String] = {
    val sequenceLines = mutable.ListBuffer[String]()
    val sequences = mutable.ListBuffer[String]()
    
    lines.foreach { line =>
      if (line.startsWith(">")) {
        if (sequenceLines.nonEmpty) {
          sequences += sequenceLines.mkString
          sequenceLines.clear()
        }
      } else {
        sequenceLines += line
      }
    }
    
    if (sequenceLines.nonEmpty) {
      sequences += sequenceLines.mkString
    }
    
    sequences.toList
  }
}
```

## Example Usage

For input like:
```
>seq1
ACGTACGT
>seq2
TGCATGCA
>seq3
CGATCGAT
```

The output would be:
```
A 4
C 4
G 4
T 4
```

## Key Features

1. **Fasta Parsing**: Properly handles FASTA format with headers
2. **Symbol Counting**: Counts occurrences of each character across all sequences
3. **Alphabetical Order**: Results are sorted by symbol for consistent output
4. **Efficient**: Uses functional programming constructs for clean, readable code

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n is the number of sequences and m is the average sequence length
- **Space Complexity**: O(k) where k is the number of unique symbols

This solution handles the Rosalind problem requirements efficiently and correctly processes multiple sequence alignments to isolate and count symbols.

