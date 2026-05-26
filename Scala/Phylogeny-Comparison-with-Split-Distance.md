# Rosalind Problem: Phylogeny Comparison with Split Distance (Scala Solution)

## Problem Understanding

The task is to compute the split distance between two phylogenetic trees. The split distance is the number of splits (edges) that are present in one tree but not in the other.

## Solution Approach

I'll implement a solution that:
1. Parses Newick format tree representations
2. Extracts splits from each tree
3. Computes the split distance as the number of symmetric differences between splits

## Implementation

```scala
import scala.collection.mutable
import scala.io.Source

object PhylogenyComparison {
  
  // Case class to represent a tree node
  case class TreeNode(name: Option[String], children: List[TreeNode])
  
  // Extract splits from a tree
  def extractSplits(node: TreeNode, allLeaves: Set[String]): Set[Set[String]] = {
    val splits = mutable.Set[Set[String]]()
    
    def dfs(current: TreeNode): Set[String] = {
      current.children match {
        case Nil => 
          // Leaf node
          current.name match {
            case Some(leafName) => Set(leafName)
            case None => Set.empty
          }
        case children => 
          // Internal node
          val childLeaves = children.map(dfs)
          val allChildLeaves = childLeaves.flatten.toSet
          
          // Add splits for each child subtree
          childLeaves.foreach { leaves =>
            if (leaves.nonEmpty) {
              splits += leaves
              splits += (allLeaves -- leaves)
            }
          }
          
          allChildLeaves
      }
    }
    
    dfs(node)
    splits.toSet
  }
  
  // Parse Newick format string into tree structure
  def parseNewick(newick: String): TreeNode = {
    // Simple parser for Newick format
    def parseHelper(s: String, index: Int): (TreeNode, Int) = {
      if (index >= s.length) throw new Exception("Invalid Newick format")
      
      val char = s(index)
      
      char match {
        case '(' =>
          // Parse subtree
          val children = mutable.ListBuffer[TreeNode]()
          var i = index + 1
          
          while (i < s.length && s(i) != ')') {
            if (s(i) == ',') {
              i += 1
            } else {
              val (child, newIndex) = parseHelper(s, i)
              children += child
              i = newIndex
            }
          }
          
          i += 1 // Skip ')'
          
          // Check if there's a node name
          val nodeName = if (i < s.length && s(i) != '(' && s(i) != ')') {
            var nameStart = i
            while (i < s.length && s(i) != ':' && s(i) != '(' && s(i) != ')' && s(i) != ',') {
              i += 1
            }
            Some(s.substring(nameStart, i))
          } else {
            None
          }
          
          (TreeNode(nodeName, children.toList), i)
          
        case _ =>
          // Leaf node
          var nameStart = index
          while (index < s.length && s(index) != ':' && s(index) != '(' && s(index) != ')' && s(index) != ',') {
            index += 1
          }
          val nodeName = s.substring(nameStart, index)
          (TreeNode(Some(nodeName), List()), index)
      }
    }
    
    // Remove semicolon and parse
    val cleanNewick = newick.stripSuffix(";")
    parseHelper(cleanNewick, 0)._1
  }
  
  // Get all leaf names from a tree
  def getLeaves(node: TreeNode): Set[String] = {
    def dfs(current: TreeNode): Set[String] = {
      current.children match {
        case Nil => 
          current.name match {
            case Some(name) => Set(name)
            case None => Set.empty
          }
        case children => 
          children.flatMap(dfs).toSet
      }
    }
    
    dfs(node)
  }
  
  // Compute split distance between two trees
  def splitDistance(tree1: TreeNode, tree2: TreeNode): Int = {
    val leaves1 = getLeaves(tree1)
    val leaves2 = getLeaves(tree2)
    
    if (leaves1 != leaves2) {
      throw new Exception("Trees must have the same leaves")
    }
    
    val splits1 = extractSplits(tree1, leaves1)
    val splits2 = extractSplits(tree2, leaves2)
    
    // Split distance is the symmetric difference
    (splits1 -- splits2).size + (splits2 -- splits1).size
  }
  
  // Alternative implementation using a more direct approach
  def computeSplitDistance(tree1Str: String, tree2Str: String): Int = {
    val tree1 = parseNewick(tree1Str)
    val tree2 = parseNewick(tree2Str)
    
    val leaves1 = getLeaves(tree1)
    val leaves2 = getLeaves(tree2)
    
    if (leaves1 != leaves2) {
      throw new Exception("Trees must have the same leaves")
    }
    
    val splits1 = extractSplits(tree1, leaves1)
    val splits2 = extractSplits(tree2, leaves2)
    
    // Count symmetric difference
    val symmetricDiff = (splits1 -- splits2) ++ (splits2 -- splits1)
    symmetricDiff.size
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val tree1 = "((a,b),(c,d));"
    val tree2 = "((a,c),(b,d));"
    
    try {
      val distance = computeSplitDistance(tree1, tree2)
      println(s"Split distance: $distance")
    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
```

## Key Components Explained

1. **TreeNode Class**: Represents tree nodes with optional names and children
2. **parseNewick**: Converts Newick format string into tree structure
3. **extractSplits**: Computes all splits (subsets of leaves) for a given tree
4. **getLeaves**: Extracts all leaf names from a tree
5. **splitDistance**: Computes the symmetric difference between splits of two trees

## Usage Example

```scala
// Example trees in Newick format
val tree1 = "((a,b),(c,d));"
val tree2 = "((a,c),(b,d));"

val distance = PhylogenyComparison.computeSplitDistance(tree1, tree2)
println(s"Split distance: $distance")
```

## Time Complexity

- **Parsing**: O(n) where n is the length of the Newick string
- **Split extraction**: O(n) where n is the number of nodes
- **Distance computation**: O(s₁ + s₂) where s₁ and s₂ are the number of splits in each tree

## Space Complexity

O(n + s₁ + s₂) for storing the tree structure and splits

This solution handles the core requirements of computing split distance between phylogenetic trees, which is essential for comparing tree topologies in phylogenetics.

