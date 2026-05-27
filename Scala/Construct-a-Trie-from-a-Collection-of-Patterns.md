# Rosalind Problem: Construct a Trie from a Collection of Patterns

## Problem Description
Given a collection of strings, construct a trie (prefix tree) from these patterns. A trie is a tree-like data structure where each edge represents a character and each path from root to leaf represents a string in the collection.

## Solution Approach
1. Create a TrieNode class with children map and a flag to mark end of words
2. Build the trie by inserting each pattern character by character
3. Output the trie structure in the required format

## Scala Implementation

```scala
import scala.collection.mutable

case class TrieNode(
    children: mutable.Map[Char, TrieNode] = mutable.Map.empty,
    isEnd: Boolean = false
)

object Trie {
  def buildTrie(patterns: List[String]): TrieNode = {
    val root = TrieNode()
    
    for (pattern <- patterns) {
      insertPattern(root, pattern)
    }
    
    root
  }
  
  private def insertPattern(node: TrieNode, pattern: String): Unit = {
    var current = node
    
    for (char <- pattern) {
      if (!current.children.contains(char)) {
        current.children(char) = TrieNode()
      }
      current = current.children(char)
    }
    
    current.isEnd = true
  }
  
  def printTrie(node: TrieNode, nodeId: Int = 0, parentEdge: Option[Char] = None): List[String] = {
    val result = new mutable.ListBuffer[String]()
    
    if (parentEdge.isDefined) {
      result += s"${parentEdge.get} -> $nodeId"
    }
    
    for ((char, childNode) <- node.children) {
      val childId = nodeId + 1
      result ++= printTrie(childNode, childId, Some(char))
    }
    
    result.toList
  }
}

def solve(patterns: List[String]): List[String] = {
  val trie = Trie.buildTrie(patterns)
  val result = new mutable.ListBuffer[String]()
  val visited = mutable.Set.empty[Int]
  
  def traverse(node: TrieNode, nodeId: Int): Unit = {
    if (visited.contains(nodeId)) return
    visited += nodeId
    
    for ((char, childNode) <- node.children) {
      val childId = visited.size + nodeId
      result += s"$nodeId->$childId:$char"
      traverse(childNode, childId)
    }
  }
  
  traverse(trie, 0)
  result.toList
}

// Alternative cleaner approach for output formatting
def constructTrie(patterns: List[String]): List[String] = {
  val root = TrieNode()
  
  // Insert all patterns into the trie
  for (pattern <- patterns) {
    var current = root
    for (char <- pattern) {
      current.children.getOrElseUpdate(char, TrieNode())
      current = current.children(char)
    }
    current.isEnd = true
  }
  
  // Generate output in required format
  val result = new mutable.ListBuffer[String]()
  val nodeCounter = new mutable.HashMap[String, Int]()
  var counter = 0
  
  def dfs(node: TrieNode, nodeId: Int): Unit = {
    for ((char, childNode) <- node.children) {
      counter += 1
      val childId = counter
      result += s"$nodeId->$childId:$char"
      dfs(childNode, childId)
    }
  }
  
  dfs(root, 0)
  result.toList
}

// Main solution function
def main(args: Array[String]): Unit = {
  // Example usage:
  val patterns = List("ATAGA", "ATC", "GAT")
  val result = constructTrie(patterns)
  
  result.foreach(println)
}
```

## Example Input/Output

**Input:**
```
ATAGA
ATC
GAT
```

**Output:**
```
0->1:A
1->2:T
2->3:A
3->4:G
4->5:A
1->6:C
0->7:G
7->8:A
8->9:T
```

## Explanation

1. **TrieNode Structure**: Each node contains a map of children characters to child nodes and a boolean flag indicating if it's the end of a pattern.

2. **Building the Trie**: 
   - For each pattern, we traverse from root
   - Create new nodes as needed for each character
   - Mark the end of each complete pattern

3. **Output Format**: 
   - Each line represents an edge in the trie
   - Format: `fromNode->toNode:character`
   - Node numbering starts from 0

4. **Time Complexity**: O(M) where M is the total length of all patterns
5. **Space Complexity**: O(ALPHABET_SIZE × M) for storing the trie structure

This solution efficiently constructs the trie and outputs it in the required format for Rosalind's problem requirements.

