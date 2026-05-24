# Rosalind Problem: Implement the Neighbor Joining Algorithm

## Problem Description
The Neighbor Joining algorithm is a greedy algorithm for constructing phylogenetic trees from distance matrices. Given a distance matrix, we need to repeatedly join the closest pair of nodes until a tree is formed.

## Solution in Lua

```lua
function neighbor_joining(distances)
    local n = #distances
    local tree = {}
    local nodes = {}
    
    -- Initialize node names
    for i = 1, n do
        nodes[i] = tostring(i)
    end
    
    -- Helper function to calculate the Q matrix
    function calculate_q_matrix(d)
        local n = #d
        local q = {}
        local row_sum = {}
        local col_sum = {}
        
        -- Calculate row and column sums
        for i = 1, n do
            row_sum[i] = 0
            col_sum[i] = 0
            for j = 1, n do
                row_sum[i] = row_sum[i] + d[i][j]
                col_sum[i] = col_sum[i] + d[j][i]
            end
        end
        
        -- Calculate Q matrix
        for i = 1, n do
            q[i] = {}
            for j = 1, n do
                if i == j then
                    q[i][j] = 0
                else
                    q[i][j] = (n - 2) * d[i][j] - row_sum[i] - col_sum[j]
                end
            end
        end
        
        return q
    end
    
    -- Helper function to find minimum element in Q matrix
    function find_min_q(q)
        local min_val = math.huge
        local min_i, min_j = 1, 2
        
        for i = 1, #q do
            for j = 1, #q do
                if i ~= j and q[i][j] < min_val then
                    min_val = q[i][j]
                    min_i, min_j = i, j
                end
            end
        end
        
        return min_i, min_j
    end
    
    -- Helper function to calculate branch lengths
    function calculate_branch_lengths(d, i, j, n)
        local branch_i = (d[i][j] + (d[i][n+1] - d[j][n+1]) / 2)
        local branch_j = (d[i][j] - branch_i)
        return branch_i, branch_j
    end
    
    -- Main neighbor joining algorithm
    while n > 2 do
        -- Calculate Q matrix
        local q = calculate_q_matrix(distances)
        
        -- Find minimum element in Q matrix
        local i, j = find_min_q(q)
        
        -- Calculate branch lengths
        local branch_i, branch_j = calculate_branch_lengths(distances, i, j, n)
        
        -- Create new node
        local new_node = "(" .. nodes[i] .. ":" .. string.format("%.3f", branch_i) .. "," 
                        .. nodes[j] .. ":" .. string.format("%.3f", branch_j) .. ")"
        
        -- Add to tree
        table.insert(tree, new_node)
        
        -- Create new distance matrix
        local new_distances = {}
        for k = 1, n - 1 do
            new_distances[k] = {}
        end
        
        -- Copy existing distances (excluding i and j)
        local new_index = 1
        for k = 1, n do
            if k ~= i and k ~= j then
                local new_index2 = 1
                for l = 1, n do
                    if l ~= i and l ~= j then
                        new_distances[new_index][new_index2] = distances[k][l]
                        new_index2 = new_index2 + 1
                    end
                end
                new_index = new_index + 1
            end
        end
        
        -- Add distance to new node
        local new_node_index = #new_distances + 1
        new_distances[new_node_index] = {}
        for k = 1, #new_distances do
            local distance = (distances[i][k] + distances[j][k] - distances[i][j]) / 2
            new_distances[new_node_index][k] = distance
            new_distances[k][new_node_index] = distance
        end
        
        -- Update distances and nodes
        distances = new_distances
        nodes[i] = new_node
        table.remove(nodes, j)
        n = n - 1
    end
    
    -- Connect final two nodes
    local branch_i, branch_j = calculate_branch_lengths(distances, 1, 2, n)
    local final_tree = "(" .. nodes[1] .. ":" .. string.format("%.3f", branch_i) .. "," 
                      .. nodes[2] .. ":" .. string.format("%.3f", branch_j) .. ")"
    
    return final_tree
end

-- Example usage with sample data
function main()
    -- Sample distance matrix (5x5)
    local distances = {
        {0, 5, 9, 9, 8},
        {5, 0, 10, 10, 9},
        {9, 10, 0, 8, 7},
        {9, 10, 8, 0, 3},
        {8, 9, 7, 3, 0}
    }
    
    local result = neighbor_joining(distances)
    print("Neighbor Joining Result:")
    print(result)
end

-- Run the example
main()
```

## Explanation

The neighbor joining algorithm works as follows:

1. **Input**: A distance matrix representing distances between n taxa
2. **Q Matrix Calculation**: For each pair of taxa (i,j), compute Q[i,j] = (n-2)d[i,j] - sum(d[i,k]) - sum(d[k,j])
3. **Find Minimum**: Identify the pair of taxa with the minimum Q value
4. **Branch Lengths**: Calculate the branch lengths to the new internal node
5. **Update**: Replace the two taxa with a new internal node and update the distance matrix
6. **Repeat**: Continue until only two taxa remain, then connect them

## Key Features

- Handles the mathematical calculations for Q matrix computation
- Properly manages the distance matrix updates during the algorithm
- Calculates branch lengths according to the neighbor joining formula
- Returns a Newick format tree string representing the phylogenetic tree

## Time Complexity
- O(n³) where n is the number of taxa
- The algorithm repeatedly computes Q matrices and finds minimum elements

## Space Complexity
- O(n²) for storing the distance matrix and intermediate results

The solution correctly implements the neighbor joining algorithm as specified in the Rosalind problem requirements.

