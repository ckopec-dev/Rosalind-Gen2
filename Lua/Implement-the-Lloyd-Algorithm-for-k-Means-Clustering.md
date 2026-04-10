# Rosalind Problem: Implement the Lloyd Algorithm for k-Means Clustering

## Problem Description
Implement the Lloyd algorithm for k-means clustering, which iteratively assigns points to clusters and updates cluster centers until convergence.

## Solution

```lua
-- Function to calculate Euclidean distance between two points
function euclidean_distance(p1, p2)
    local sum = 0
    for i = 1, #p1 do
        sum = sum + (p1[i] - p2[i])^2
    end
    return math.sqrt(sum)
end

-- Function to calculate centroid of a set of points
function calculate_centroid(points)
    if #points == 0 then return nil end
    
    local centroid = {}
    local dim = #points[1]
    
    -- Initialize centroid with zeros
    for i = 1, dim do
        centroid[i] = 0
    end
    
    -- Sum all coordinates
    for _, point in ipairs(points) do
        for i = 1, dim do
            centroid[i] = centroid[i] + point[i]
        end
    end
    
    -- Divide by number of points to get average
    for i = 1, dim do
        centroid[i] = centroid[i] / #points
    end
    
    return centroid
end

-- Function to find the closest centroid to a point
function closest_centroid(point, centroids)
    local min_distance = math.huge
    local closest_index = 1
    
    for i, centroid in ipairs(centroids) do
        local distance = euclidean_distance(point, centroid)
        if distance < min_distance then
            min_distance = distance
            closest_index = i
        end
    end
    
    return closest_index
end

-- Function to implement Lloyd's k-means algorithm
function lloyd_kmeans(points, k, max_iter)
    -- Initialize centroids randomly from the points
    local centroids = {}
    local used_indices = {}
    
    -- Select k random points as initial centroids
    for i = 1, k do
        local index
        repeat
            index = math.random(1, #points)
        until not used_indices[index]
        used_indices[index] = true
        table.insert(centroids, points[index])
    end
    
    -- Main iteration loop
    for iteration = 1, max_iter do
        -- Assign points to clusters
        local clusters = {}
        for i = 1, k do
            clusters[i] = {}
        end
        
        for _, point in ipairs(points) do
            local cluster_id = closest_centroid(point, centroids)
            table.insert(clusters[cluster_id], point)
        end
        
        -- Update centroids
        local new_centroids = {}
        local converged = true
        
        for i = 1, k do
            local new_centroid = calculate_centroid(clusters[i])
            if new_centroid then
                table.insert(new_centroids, new_centroid)
                -- Check if centroid changed significantly
                if #centroids >= i then
                    local distance = euclidean_distance(centroids[i], new_centroid)
                    if distance > 1e-6 then
                        converged = false
                    end
                end
            else
                -- If cluster is empty, keep old centroid
                table.insert(new_centroids, centroids[i])
            end
        end
        
        centroids = new_centroids
        
        -- Check for convergence
        if converged then
            break
        end
    end
    
    return centroids
end

-- Function to format centroid output
function format_centroid(centroid)
    local result = {}
    for i, coord in ipairs(centroid) do
        table.insert(result, string.format("%.3f", coord))
    end
    return table.concat(result, " ")
end

-- Main execution function
function main()
    -- Example input (replace with actual input reading)
    local points = {
        {1.3, 1.1},
        {1.3, 0.2},
        {0.6, 2.8},
        {3.0, 3.2},
        {1.2, 0.7},
        {1.4, 1.6},
        {1.2, 1.0},
        {1.1, 1.6},
        {2.6, 3.9},
        {2.3, 2.7}
    }
    
    local k = 2
    local max_iter = 100
    
    -- Run Lloyd's algorithm
    local final_centroids = lloyd_kmeans(points, k, max_iter)
    
    -- Print results
    for _, centroid in ipairs(final_centroids) do
        print(format_centroid(centroid))
    end
end

-- Run the main function
main()
```

## Explanation

This implementation follows the Lloyd algorithm for k-means clustering:

1. **Initialization**: Randomly select k points as initial centroids
2. **Assignment**: Assign each point to the nearest centroid
3. **Update**: Recalculate centroids as the mean of all points in each cluster
4. **Repeat**: Continue until convergence or maximum iterations reached

## Key Functions

- `euclidean_distance()`: Calculates distance between two points
- `calculate_centroid()`: Computes the centroid of a cluster
- `closest_centroid()`: Finds the nearest centroid to a point
- `lloyd_kmeans()`: Main algorithm implementation
- `format_centroid()`: Formats output with proper decimal places

## Time Complexity
- O(n × k × i × d) where n is number of points, k is number of clusters, i is iterations, d is dimensions

## Space Complexity
- O(n × d) for storing points and clusters

The algorithm will converge when centroids no longer change significantly or reach the maximum iteration limit.

