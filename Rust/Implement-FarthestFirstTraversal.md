# Rosalind Problem: Implement_FarthestFirstTraversal

This problem asks us to implement the Farthest First Traversal algorithm for clustering. The algorithm starts with one point and repeatedly adds the point that is farthest from all currently selected points.

## Problem Understanding

1. **Input**: A collection of points in k-dimensional space and a number k (number of clusters)
2. **Output**: The first k points selected by the Farthest First Traversal algorithm
3. **Algorithm**: 
   - Start with any point as the first cluster center
   - For each subsequent cluster center, select the point that is farthest from all existing cluster centers
   - Continue until k cluster centers are selected

## Solution Approach

1. Implement Euclidean distance calculation between points
2. Implement the Farthest First Traversal algorithm
3. Handle the selection process correctly

## Rust Implementation

```rust
fn euclidean_distance(p1: &[f64], p2: &[f64]) -> f64 {
    p1.iter()
        .zip(p2.iter())
        .map(|(x, y)| (x - y).powi(2))
        .sum::<f64>()
        .sqrt()
}

fn farthest_first_traversal(points: &[Vec<f64>], k: usize) -> Vec<Vec<f64>> {
    if points.is_empty() || k == 0 {
        return vec![];
    }
    
    // Start with the first point as the first center
    let mut centers = vec![points[0].clone()];
    
    // Continue until we have k centers
    while centers.len() < k {
        let mut max_distance = f64::NEG_INFINITY;
        let mut farthest_point = &points[0];
        
        // Find the point that is farthest from all existing centers
        for point in points {
            let min_distance_to_centers = centers
                .iter()
                .map(|center| euclidean_distance(point, center))
                .min_by(|a, b| a.partial_cmp(b).unwrap())
                .unwrap();
            
            if min_distance_to_centers > max_distance {
                max_distance = min_distance_to_centers;
                farthest_point = point;
            }
        }
        
        centers.push(farthest_point.clone());
    }
    
    centers
}

fn main() {
    // Example usage with sample data
    let points = vec![
        vec![0.0, 0.0],
        vec![1.0, 0.0],
        vec![0.0, 1.0],
        vec![1.0, 1.0],
        vec![2.0, 2.0],
        vec![3.0, 3.0],
    ];
    
    let k = 3;
    let result = farthest_first_traversal(&points, k);
    
    println!("Selected centers:");
    for center in result {
        println!("{:?}", center);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_euclidean_distance() {
        let p1 = vec![0.0, 0.0];
        let p2 = vec![3.0, 4.0];
        let distance = euclidean_distance(&p1, &p2);
        assert_eq!(distance, 5.0);
    }

    #[test]
    fn test_farthest_first_traversal() {
        let points = vec![
            vec![0.0, 0.0],
            vec![1.0, 0.0],
            vec![0.0, 1.0],
            vec![1.0, 1.0],
            vec![2.0, 2.0],
        ];
        
        let result = farthest_first_traversal(&points, 3);
        assert_eq!(result.len(), 3);
        // The exact result depends on the algorithm's implementation details
        // but should contain 3 points
    }
}
```

## Algorithm Explanation

1. **Distance Calculation**: The `euclidean_distance` function computes the Euclidean distance between two points in k-dimensional space using the formula: √(Σ(xi - yi)²)

2. **Main Algorithm**:
   - Start with the first point as the first cluster center
   - For each remaining cluster center:
     - Examine all unselected points
     - For each point, calculate the minimum distance to all existing centers
     - Select the point with the maximum minimum distance
   - Continue until k centers are selected

3. **Time Complexity**: O(n²k) where n is the number of points and k is the number of clusters

## Key Insights

- The algorithm is greedy and doesn't guarantee the optimal clustering solution
- It's commonly used as a heuristic for k-means initialization
- The selection process ensures good spread of cluster centers
- The algorithm works for any number of dimensions

## Usage

To use this with Rosalind's input format, you would typically:
1. Parse the input file to get points and k value
2. Call `farthest_first_traversal` with the parsed data
3. Format and output the results as required by the problem specification

The implementation handles edge cases like empty inputs and ensures proper selection of cluster centers according to the Farthest First Traversal algorithm.

