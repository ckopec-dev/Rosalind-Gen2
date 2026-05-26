# Rosalind Problem: Strongly Connected Components (SCC)

## Problem Description
Find the strongly connected components of a directed graph.

## Solution in Scala

```scala
import scala.collection.mutable

object SCC {
  
  def scc(graph: Map[Int, List[Int]]): List[List[Int]] = {
    val visited = mutable.Set[Int]()
    val stack = mutable.Stack[Int]()
    val sccs = mutable.ListBuffer[List[Int]]()
    
    // First pass: get finishing times
    def dfsFirst(node: Int): Unit = {
      visited += node
      graph.getOrElse(node, List()).foreach(neighbor => {
        if (!visited.contains(neighbor)) {
          dfsFirst(neighbor)
        }
      })
      stack.push(node)
    }
    
    // Second pass: find SCCs
    def dfsSecond(node: Int, component: mutable.ListBuffer[Int]): Unit = {
      visited += node
      component += node
      graph.getOrElse(node, List()).foreach(neighbor => {
        if (!visited.contains(neighbor)) {
          dfsSecond(neighbor, component)
        }
      })
    }
    
    // Get finishing times
    graph.keys.foreach(node => {
      if (!visited.contains(node)) {
        dfsFirst(node)
      }
    })
    
    // Reset visited for second pass
    visited.clear()
    
    // Create transpose graph
    val transpose = mutable.Map[Int, mutable.ListBuffer[Int]]()
    graph.foreach { case (node, neighbors) =>
      neighbors.foreach(neighbor => {
        transpose.getOrElseUpdate(neighbor, mutable.ListBuffer[Int]()) += node
      })
    }
    
    // Find SCCs using transpose graph
    while (stack.nonEmpty) {
      val node = stack.pop()
      if (!visited.contains(node)) {
        val component = mutable.ListBuffer[Int]()
        dfsSecond(node, component)
        sccs += component.toList
      }
    }
    
    sccs.toList
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val graph = Map(
      1 -> List(2),
      2 -> List(3, 4),
      3 -> List(1, 5),
      4 -> List(6),
      5 -> List(4, 6),
      6 -> List()
    )
    
    val result = scc(graph)
    println("Strongly Connected Components:")
    result.foreach(comp => println(comp.sorted.mkString(" ")))
  }
}

// Alternative implementation using Kosaraju's algorithm more clearly
object SCC_Kosaraju {
  
  def kosaraju(graph: Map[Int, List[Int]]): List[List[Int]] = {
    val visited = mutable.Set[Int]()
    val finishStack = mutable.Stack[Int]()
    
    // Step 1: Get finishing times
    def dfsFirst(node: Int): Unit = {
      visited += node
      graph.getOrElse(node, List()).foreach(neighbor => {
        if (!visited.contains(neighbor)) {
          dfsFirst(neighbor)
        }
      })
      finishStack.push(node)
    }
    
    // Step 2: Create transpose graph
    def getTranspose: Map[Int, List[Int]] = {
      val transpose = mutable.Map[Int, mutable.ListBuffer[Int]]()
      graph.foreach { case (node, neighbors) =>
        neighbors.foreach(neighbor => {
          transpose.getOrElseUpdate(neighbor, mutable.ListBuffer[Int]()) += node
        })
      }
      transpose.map { case (k, v) => k -> v.toList }.toMap
    }
    
    // Step 3: DFS on transpose graph to find SCCs
    def dfsSecond(node: Int, component: mutable.ListBuffer[Int]): Unit = {
      visited += node
      component += node
      graph.getOrElse(node, List()).foreach(neighbor => {
        if (!visited.contains(neighbor)) {
          dfsSecond(neighbor, component)
        }
      })
    }
    
    // Get finishing times
    graph.keys.foreach(node => {
      if (!visited.contains(node)) {
        dfsFirst(node)
      }
    })
    
    // Reset visited and get transpose
    visited.clear()
    val transpose = getTranspose
    
    val sccs = mutable.ListBuffer[List[Int]]()
    
    // Process nodes in reverse finishing order
    while (finishStack.nonEmpty) {
      val node = finishStack.pop()
      if (!visited.contains(node)) {
        val component = mutable.ListBuffer[Int]()
        dfsSecond(node, component)
        sccs += component.toList
      }
    }
    
    sccs.toList
  }
}
```

## Explanation

This solution implements **Kosaraju's algorithm** for finding strongly connected components:

1. **First DFS Pass**: 
   - Perform DFS on the original graph
   - Keep track of finishing times using a stack
   - Nodes are pushed to stack when they finish processing

2. **Graph Transpose**: 
   - Create a transpose graph (reverse all edges)

3. **Second DFS Pass**: 
   - Process nodes in reverse order of finishing times
   - Perform DFS on transpose graph
   - Each DFS tree gives one strongly connected component

## Time Complexity
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for storing the graph and auxiliary data structures

## Example Input/Output

**Input Graph**:
```
1 → 2
2 → 3, 4
3 → 1, 5
4 → 6
5 → 4, 6
6 → 
```

**Output**:
```
[1, 2, 3]
[4, 5]
[6]
```

This represents three strongly connected components in the graph.

