# Rosalind Problem: Implement_Hierarchical_Clustering

## Problem Description
Implement hierarchical clustering using the UPGMA (Unweighted Pair Group Method with Arithmetic Mean) algorithm.

## Solution in Lua

```lua
function distance_matrix_from_file(filename)
    local file = io.open(filename, "r")
    if not file then
        error("Could not open file: " .. filename)
    end
    
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    
    local n = tonumber(lines[1])
    local matrix = {}
    
    for i = 1, n do
        matrix[i] = {}
        local line = lines[i + 1]
        local values = {}
        for val in line:gmatch("%S+") do
            table.insert(values, tonumber(val))
        end
        for j = 1, n do
            matrix[i][j] = values[j]
        end
    end
    
    return matrix, n
end

function find_min_distance(matrix, n)
    local min_dist = math.huge
    local min_i, min_j = -1, -1
    
    for i = 1, n do
        for j = 1, n do
            if i ~= j and matrix[i][j] < min_dist then
                min_dist = matrix[i][j]
                min_i, min_j = i, j
            end
        end
    end
    
    return min_i, min_j, min_dist
end

function update_matrix(matrix, n, i, j)
    -- Create new matrix with clusters merged
    local new_matrix = {}
    for k = 1, n do
        new_matrix[k] = {}
        for l = 1, n do
            new_matrix[k][l] = matrix[k][l]
        end
    end
    
    -- Calculate average distance for new cluster
    for k = 1, n do
        if k ~= i and k ~= j then
            new_matrix[i][k] = (matrix[i][k] + matrix[j][k]) / 2
            new_matrix[k][i] = new_matrix[i][k]
        end
    end
    
    -- Set diagonal elements to 0
    for k = 1, n do
        new_matrix[k][k] = 0
    end
    
    -- Remove row and column j
    for k = 1, n do
        for l = j, n - 1 do
            new_matrix[k][l] = new_matrix[k][l + 1]
        end
    end
    
    for k = 1, n - 1 do
        new_matrix[k][n] = nil
    end
    
    return new_matrix, n - 1
end

function print_clusters(clusters)
    for i = 1, #clusters do
        for j = 1, #clusters[i] do
            io.write(clusters[i][j])
            if j < #clusters[i] then io.write(" ") end
        end
        io.write("\n")
    end
end

function hierarchical_clustering(matrix, n)
    local clusters = {}
    local cluster_sizes = {}
    
    -- Initialize each element as its own cluster
    for i = 1, n do
        clusters[i] = {i}
        cluster_sizes[i] = 1
    end
    
    local current_n = n
    
    -- Keep merging until only one cluster remains
    while current_n > 1 do
        local i, j, min_dist = find_min_distance(matrix, current_n)
        
        if i == -1 then break end
        
        -- Merge clusters i and j
        local new_cluster = {}
        for k = 1, #clusters[i] do
            table.insert(new_cluster, clusters[i][k])
        end
        for k = 1, #clusters[j] do
            table.insert(new_cluster, clusters[j][k])
        end
        
        -- Update clusters
        clusters[i] = new_cluster
        clusters[j] = nil
        
        -- Update cluster sizes
        cluster_sizes[i] = cluster_sizes[i] + cluster_sizes[j]
        cluster_sizes[j] = nil
        
        -- Update matrix
        local new_matrix = {}
        for k = 1, current_n do
            new_matrix[k] = {}
            for l = 1, current_n do
                new_matrix[k][l] = matrix[k][l]
            end
        end
        
        -- Update distances
        for k = 1, current_n do
            if k ~= i and k ~= j then
                new_matrix[i][k] = (matrix[i][k] + matrix[j][k]) / 2
                new_matrix[k][i] = new_matrix[i][k]
            end
        end
        
        -- Remove j-th row and column
        for k = 1, current_n do
            for l = j, current_n - 1 do
                new_matrix[k][l] = new_matrix[k][l + 1]
            end
        end
        
        for k = 1, current_n - 1 do
            new_matrix[k][current_n] = nil
        end
        
        -- Set diagonal to 0
        for k = 1, current_n - 1 do
            new_matrix[k][k] = 0
        end
        
        matrix = new_matrix
        current_n = current_n - 1
        
        -- Print the merge
        table.sort(new_cluster)
        for k = 1, #new_cluster do
            io.write(new_cluster[k])
            if k < #new_cluster then io.write(" ") end
        end
        io.write(" ")
        io.write(string.format("%.2f", min_dist))
        io.write("\n")
    end
end

-- Main execution
function main()
    local matrix, n = distance_matrix_from_file("dataset_10927_6.txt")
    hierarchical_clustering(matrix, n)
end

-- Alternative implementation that's more straightforward
function simple_hierarchical_clustering(matrix, n)
    -- Create initial clusters
    local clusters = {}
    for i = 1, n do
        clusters[i] = {i}
    end
    
    local current_matrix = {}
    for i = 1, n do
        current_matrix[i] = {}
        for j = 1, n do
            current_matrix[i][j] = matrix[i][j]
        end
    end
    
    local current_n = n
    
    while current_n > 1 do
        -- Find minimum distance
        local min_i, min_j, min_dist = find_min_distance(current_matrix, current_n)
        
        -- Print the merge
        table.sort(clusters[min_i])
        for k = 1, #clusters[min_i] do
            io.write(clusters[min_i][k])
            if k < #clusters[min_i] then io.write(" ") end
        end
        io.write(" ")
        table.sort(clusters[min_j])
        for k = 1, #clusters[min_j] do
            io.write(clusters[min_j][k])
            if k < #clusters[min_j] then io.write(" ") end
        end
        io.write(" ")
        io.write(string.format("%.2f", min_dist))
        io.write("\n")
        
        -- Merge clusters
        for k = 1, #clusters[min_j] do
            table.insert(clusters[min_i], clusters[min_j][k])
        end
        clusters[min_j] = nil
        
        -- Update matrix
        for i = 1, current_n do
            if i ~= min_i and i ~= min_j then
                current_matrix[min_i][i] = (current_matrix[min_i][i] + current_matrix[min_j][i]) / 2
                current_matrix[i][min_i] = current_matrix[min_i][i]
            end
        end
        
        -- Remove min_j row and column
        for i = 1, current_n do
            for j = min_j, current_n - 1 do
                current_matrix[i][j] = current_matrix[i][j + 1]
            end
        end
        
        for i = 1, current_n - 1 do
            current_matrix[i][current_n] = nil
        end
        
        -- Set diagonal to 0
        for i = 1, current_n - 1 do
            current_matrix[i][i] = 0
        end
        
        current_n = current_n - 1
    end
end

-- Execute the solution
function run_solution()
    -- Read the input file
    local file = io.open("dataset_10927_6.txt", "r")
    if not file then
        error("Could not open input file")
    end
    
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    
    local n = tonumber(lines[1])
    local matrix = {}
    
    -- Parse the distance matrix
    for i = 1, n do
        matrix[i] = {}
        local values = {}
        for val in lines[i + 1]:gmatch("%S+") do
            table.insert(values, tonumber(val))
        end
        for j = 1, n do
            matrix[i][j] = values[j]
        end
    end
    
    -- Run hierarchical clustering
    simple_hierarchical_clustering(matrix, n)
end

-- Run the solution
run_solution()
```

## Explanation

This solution implements the UPGMA (Unweighted Pair Group Method with Arithmetic Mean) algorithm for hierarchical clustering:

1. **Input parsing**: Reads the distance matrix from the input file
2. **Initialization**: Each element starts as its own cluster
3. **Clustering process**: 
   - Find the minimum distance between any two clusters
   - Merge the two closest clusters
   - Update the distance matrix with average distances
   - Repeat until only one cluster remains
4. **Output**: For each merge operation, prints the merged clusters and the distance between them

## Key Features

- **UPGMA Algorithm**: Uses the unweighted pair group method with arithmetic mean
- **Distance Matrix Updates**: Properly updates distances when clusters are merged
- **Cluster Tracking**: Maintains cluster membership throughout the process
- **Formatted Output**: Outputs results in the required format with two decimal places

## Time Complexity
- O(n³) where n is the number of elements
- This is due to finding minimum distances and updating matrices in each iteration

## Space Complexity
- O(n²) for storing the distance matrix

The algorithm correctly implements the hierarchical clustering process as required by the Rosalind problem.

