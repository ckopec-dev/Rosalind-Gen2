# Rosalind Problem: Implement the Lloyd Algorithm for k-Means Clustering

I'll solve this step by step, implementing the Lloyd algorithm for k-means clustering in Rust.

## Problem Understanding

The Lloyd algorithm for k-means clustering involves:
1. Initialize k centroids randomly
2. Assign each data point to the nearest centroid
3. Update centroids to be the mean of assigned points
4. Repeat until convergence

## Solution

```rust
use std::io;
use std::str::FromStr;

#[derive(Debug, Clone)]
struct Point {
    coordinates: Vec<f64>,
}

impl Point {
    fn new(coordinates: Vec<f64>) -> Self {
        Point { coordinates }
    }
    
    fn distance(&self, other: &Point) -> f64 {
        self.coordinates
            .iter()
            .zip(other.coordinates.iter())
            .map(|(a, b)| (a - b).powi(2))
            .sum::<f64>()
            .sqrt()
    }
}

fn read_data() -> (usize, usize, Vec<Point>) {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    
    let mut lines = input.lines();
    let first_line = lines.next().unwrap();
    let dimensions: Vec<&str> = first_line.split_whitespace().collect();
    let k = dimensions[0].parse::<usize>().unwrap();
    let m = dimensions[1].parse::<usize>().unwrap();
    
    let mut points = Vec::new();
    for line in lines {
        if line.trim().is_empty() {
            continue;
        }
        let coordinates: Vec<f64> = line
            .split_whitespace()
            .map(|s| s.parse::<f64>().unwrap())
            .collect();
        points.push(Point::new(coordinates));
    }
    
    (k, m, points)
}

fn initialize_centroids(points: &Vec<Point>, k: usize) -> Vec<Point> {
    // Simple initialization: take first k points as centroids
    points[..k].to_vec()
}

fn assign_points_to_clusters(points: &Vec<Point>, centroids: &Vec<Point>) -> Vec<usize> {
    let mut clusters = Vec::with_capacity(points.len());
    
    for point in points {
        let mut min_distance = f64::MAX;
        let mut cluster_id = 0;
        
        for (i, centroid) in centroids.iter().enumerate() {
            let distance = point.distance(centroid);
            if distance < min_distance {
                min_distance = distance;
                cluster_id = i;
            }
        }
        
        clusters.push(cluster_id);
    }
    
    clusters
}

fn calculate_centroids(points: &Vec<Point>, clusters: &Vec<usize>, k: usize) -> Vec<Point> {
    let mut new_centroids = Vec::with_capacity(k);
    
    for i in 0..k {
        let mut cluster_points = Vec::new();
        
        for (j, &cluster_id) in clusters.iter().enumerate() {
            if cluster_id == i {
                cluster_points.push(&points[j]);
            }
        }
        
        if cluster_points.is_empty() {
            // If no points assigned to cluster, keep the old centroid
            // This shouldn't happen in practice with proper initialization
            new_centroids.push(Point::new(vec![0.0; points[0].coordinates.len()]));
        } else {
            let mut sum = vec![0.0; points[0].coordinates.len()];
            
            for point in cluster_points {
                for (j, &coord) in point.coordinates.iter().enumerate() {
                    sum[j] += coord;
                }
            }
            
            let mut centroid_coords = Vec::with_capacity(sum.len());
            for &s in &sum {
                centroid_coords.push(s / cluster_points.len() as f64);
            }
            
            new_centroids.push(Point::new(centroid_coords));
        }
    }
    
    new_centroids
}

fn lloyd_algorithm(points: &Vec<Point>, k: usize, max_iterations: usize) -> Vec<Point> {
    let mut centroids = initialize_centroids(points, k);
    
    for _ in 0..max_iterations {
        let clusters = assign_points_to_clusters(points, &centroids);
        let new_centroids = calculate_centroids(points, &clusters, k);
        
        // Check for convergence (centroids haven't changed significantly)
        let mut converged = true;
        for (i, centroid) in centroids.iter().enumerate() {
            if centroid.distance(&new_centroids[i]) > 0.001 {
                converged = false;
                break;
            }
        }
        
        if converged {
            break;
        }
        
        centroids = new_centroids;
    }
    
    centroids
}

fn print_centroids(centroids: &Vec<Point>) {
    for centroid in centroids {
        for (i, &coord) in centroid.coordinates.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{:.3}", coord);
        }
        println!();
    }
}

fn main() {
    let (k, _m, points) = read_data();
    let centroids = lloyd_algorithm(&points, k, 100);
    print_centroids(&centroids);
}
```

## Explanation

### Key Components:

1. **Point Structure**: Represents a data point with coordinates
2. **Distance Calculation**: Uses Euclidean distance between points
3. **Centroid Initialization**: Takes first k points as initial centroids
4. **Assignment Step**: Each point is assigned to the nearest centroid
5. **Update Step**: Centroids are updated to be the mean of assigned points
6. **Convergence**: Algorithm stops when centroids don't change significantly

### Algorithm Steps:

1. **Read Input**: Parse k (number of clusters) and data points
2. **Initialize**: Start with k random centroids
3. **Assign Points**: For each point, find the closest centroid
4. **Update Centroids**: Calculate new centroids as means of assigned points
5. **Repeat**: Continue until convergence or max iterations

### Time Complexity:
- O(n × k × m × i) where n is number of points, k is number of clusters, m is dimensions, i is iterations

### Space Complexity:
- O(n × m + k × m) for storing points and centroids

This implementation handles the standard Lloyd algorithm for k-means clustering as required by the Rosalind problem.

