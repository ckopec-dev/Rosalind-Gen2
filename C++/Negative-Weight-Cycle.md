# Rosalind Problem: Negative Weight Cycle (Negative_Weight_Cycle)

## Problem Description
Given: A weighted graph where edges can have negative weights. Return: 1 if the graph contains a negative weight cycle, 0 otherwise.

## Approach
I'll use the Bellman-Ford algorithm to detect negative weight cycles. The key insight is that if we can relax edges more than (V-1) times, then there must be a negative cycle.

## Solution

```cpp
#include <iostream>
#include <vector>
#include <climits>
using namespace std;

struct Edge {
    int from, to, weight;
    Edge(int f, int t, int w) : from(f), to(t), weight(w) {}
};

class Graph {
private:
    int vertices;
    vector<Edge> edges;
    
public:
    Graph(int v) : vertices(v) {}
    
    void addEdge(int from, int to, int weight) {
        edges.push_back(Edge(from, to, weight));
    }
    
    int hasNegativeCycle() {
        // Initialize distances from source (vertex 0)
        vector<long long> dist(vertices, LLONG_MAX);
        dist[0] = 0;
        
        // Relax edges (V-1) times
        for (int i = 0; i < vertices - 1; i++) {
            for (const Edge& edge : edges) {
                if (dist[edge.from] != LLONG_MAX && 
                    dist[edge.from] + edge.weight < dist[edge.to]) {
                    dist[edge.to] = dist[edge.from] + edge.weight;
                }
            }
        }
        
        // Check for negative cycles by relaxing one more time
        for (const Edge& edge : edges) {
            if (dist[edge.from] != LLONG_MAX && 
                dist[edge.from] + edge.weight < dist[edge.to]) {
                return 1; // Negative cycle found
            }
        }
        
        return 0; // No negative cycle
    }
};

int main() {
    // Read input
    int n, m;
    cin >> n >> m;
    
    Graph graph(n);
    
    // Read edges
    for (int i = 0; i < m; i++) {
        int from, to, weight;
        cin >> from >> to >> weight;
        graph.addEdge(from - 1, to - 1, weight); // Convert to 0-indexed
    }
    
    // Check for negative weight cycle
    cout << graph.hasNegativeCycle() << endl;
    
    return 0;
}
```

## Explanation

1. **Graph Representation**: I use an edge list representation to store the graph, which is efficient for this problem.

2. **Bellman-Ford Algorithm**: 
   - Initialize distances from a source vertex to all other vertices as infinity, except the source itself which is 0
   - Relax all edges exactly (V-1) times, where V is the number of vertices
   - If we can still relax edges after (V-1) iterations, then there's a negative cycle

3. **Negative Cycle Detection**: 
   - After (V-1) iterations, if we can still find a shorter path, it means there's a negative cycle reachable from the source
   - This is because in a graph without negative cycles, (V-1) iterations are sufficient to find all shortest paths

4. **Time Complexity**: O(V×E) where V is vertices and E is edges
5. **Space Complexity**: O(V+E)

## Sample Input/Output

**Input:**
```
3 3
1 2 -1
2 3 -2
3 1 -3
```

**Output:**
```
1
```

This indicates there is a negative weight cycle in the graph.

**Input:**
```
3 3
1 2 1
2 3 1
3 1 1
```

**Output:**
```
0
```

This indicates there is no negative weight cycle in the graph.

