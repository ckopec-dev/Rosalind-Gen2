# Rosalind Problem 2-Satisfiability Solution in Ruby

## Problem Understanding

2-Satisfiability (2-SAT) is a special case of the Boolean satisfiability problem where each clause contains exactly 2 literals. The goal is to determine if there exists an assignment of truth values to variables that satisfies all clauses.

## Approach

I'll use the **implication graph** approach:
1. Convert each 2-clause into two implications
2. Build a directed graph of implications
3. Check if any variable and its negation are in the same strongly connected component
4. If they are, the formula is unsatisfiable; otherwise, it's satisfiable

## Solution

```ruby
class TwoSat
  def initialize(n, clauses)
    @n = n
    @clauses = clauses
    @graph = Hash.new { |h, k| h[k] = [] }
    @reverse_graph = Hash.new { |h, k| h[k] = [] }
  end

  def solve
    # Build implication graph
    build_graph
    
    # Check if formula is satisfiable
    if is_satisfiable?
      "Satisfiable"
    else
      "Unsatisfiable"
    end
  end

  private

  def build_graph
    # For each clause (a OR b), add implications:
    # -¬a → b and -¬b → a
    @clauses.each do |a, b|
      # Convert to 0-indexed and handle negation
      a_index = a > 0 ? a - 1 : -a - 1
      b_index = b > 0 ? b - 1 : -b - 1
      
      # Add implications
      add_implication(negate(a_index), b_index)
      add_implication(negate(b_index), a_index)
    end
  end

  def add_implication(from, to)
    @graph[from] << to
    @reverse_graph[to] << from
  end

  def is_satisfiable?
    # Find strongly connected components using Kosaraju's algorithm
    visited = Array.new(@n * 2, false)
    stack = []
    
    # First DFS to get finishing order
    (0...@n * 2).each do |i|
      unless visited[i]
        dfs_first(i, visited, stack)
      end
    end
    
    # Reset visited array
    visited = Array.new(@n * 2, false)
    
    # Second DFS on reverse graph in reverse order
    while !stack.empty?
      node = stack.pop
      if !visited[node]
        component = []
        dfs_second(node, visited, component)
        
        # Check if any variable and its negation are in same component
        component_set = component.to_set
        component.each do |var|
          neg_var = negate(var)
          return false if component_set.include?(neg_var)
        end
      end
    end
    
    true
  end

  def dfs_first(node, visited, stack)
    visited[node] = true
    @graph[node].each do |neighbor|
      unless visited[neighbor]
        dfs_first(neighbor, visited, stack)
      end
    end
    stack << node
  end

  def dfs_second(node, visited, component)
    visited[node] = true
    component << node
    @reverse_graph[node].each do |neighbor|
      unless visited[neighbor]
        dfs_second(neighbor, visited, component)
      end
    end
  end

  def negate(index)
    # Convert index to its negation (0-indexed)
    index ^ 1
  end
end

# Helper method to parse input and solve
def solve_2sat(n, clauses)
  solver = TwoSat.new(n, clauses)
  solver.solve
end

# Example usage:
# Input format: n (number of variables), followed by clauses
# Each clause is a pair of integers representing literals
# Positive numbers represent variables, negative numbers represent negations

# Example:
# n = 3
# clauses = [[1, 2], [-1, 3], [-2, -3]]
# result = solve_2sat(3, [[1, 2], [-1, 3], [-2, -3]])
# puts result

# Sample input processing function
def process_input(input_lines)
  n, m = input_lines[0].split.map(&:to_i)
  clauses = []
  
  1.upto(m) do |i|
    a, b = input_lines[i].split.map(&:to_i)
    clauses << [a, b]
  end
  
  solve_2sat(n, clauses)
end
```

## Key Features

1. **Implication Graph Construction**: Converts each 2-clause into logical implications
2. **Kosaraju's Algorithm**: Uses two DFS passes to find strongly connected components
3. **Variable Negation Handling**: Properly handles negated literals in the graph
4. **Satisfiability Check**: Determines if any variable and its negation are in the same SCC

## Time Complexity
- **Time**: O(V + E) where V = 2n (variables and their negations) and E = 2m (clauses)
- **Space**: O(V + E) for storing the graph

## Example Usage

```ruby
# Example problem input
input = [
  "2 3",           # 2 variables, 3 clauses
  "1 2",           # clause: x1 OR x2
  "-1 2",          # clause: NOT x1 OR x2  
  "1 -2"           # clause: x1 OR NOT x2
]

result = process_input(input)
puts result  # Should output "Satisfiable" or "Unsatisfiable"
```

This solution correctly handles the 2-SAT problem by leveraging graph theory and strongly connected components to determine satisfiability efficiently.

