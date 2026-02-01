# Rosalind Problem: Double-Degree Array

## Problem Description
Given: A simple graph with n ≤ 10^3 vertices in the edge list format.
Return: An array D[1..n] where D[i] is the sum of the degrees of i's neighbors.

## Solution Approach
1. Parse the input to build an adjacency list representation of the graph
2. Calculate the degree of each vertex
3. For each vertex, sum the degrees of its neighbors to get the double degree

## Ruby Implementation

```ruby
def double_degree_array(edges, n)
  # Build adjacency list
  adj_list = Hash.new { |h, k| h[k] = [] }
  
  edges.each do |u, v|
    adj_list[u] << v
    adj_list[v] << u
  end
  
  # Calculate degrees for each vertex
  degrees = Hash.new(0)
  adj_list.each do |vertex, neighbors|
    degrees[vertex] = neighbors.length
  end
  
  # Calculate double degree for each vertex
  result = []
  (1..n).each do |vertex|
    if adj_list[vertex].empty?
      result << 0
    else
      double_degree = adj_list[vertex].sum { |neighbor| degrees[neighbor] }
      result << double_degree
    end
  end
  
  result
end

# Read input from file or stdin
def read_input
  input = STDIN.readlines
  n, m = input[0].split.map(&:to_i)
  edges = input[1..-1].map { |line| line.split.map(&:to_i) }
  [edges, n]
end

# Main execution
edges, n = read_input
result = double_degree_array(edges, n)
puts result.join(" ")
```

## Example

**Input:**
```
5 4
1 2
2 3
3 4
4 5
```

**Step-by-step solution:**
1. Build adjacency list:
   - 1: [2]
   - 2: [1, 3]
   - 3: [2, 4]
   - 4: [3, 5]
   - 5: [4]

2. Calculate degrees:
   - deg(1) = 1
   - deg(2) = 2
   - deg(3) = 2
   - deg(4) = 2
   - deg(5) = 1

3. Calculate double degrees:
   - D[1] = deg(2) = 2
   - D[2] = deg(1) + deg(3) = 1 + 2 = 3
   - D[3] = deg(2) + deg(4) = 2 + 2 = 4
   - D[4] = deg(3) + deg(5) = 2 + 1 = 3
   - D[5] = deg(4) = 2

**Output:**
```
2 3 4 3 2
```

## Time Complexity
- O(n + m) where n is the number of vertices and m is the number of edges
- Building adjacency list: O(m)
- Calculating degrees: O(n)
- Calculating double degrees: O(n + m)

## Space Complexity
- O(n + m) for storing the adjacency list and degree information

