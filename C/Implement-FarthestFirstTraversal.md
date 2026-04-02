```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Structure to represent a point in 2D space
typedef struct {
    double x;
    double y;
} Point;

// Function to calculate Euclidean distance between two points
double distance(Point a, Point b) {
    return sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
}

// Function to find the minimum distance from a point to any point in the centers array
double min_distance(Point point, Point* centers, int num_centers) {
    if (num_centers == 0) return 0;
    
    double min_dist = distance(point, centers[0]);
    for (int i = 1; i < num_centers; i++) {
        double dist = distance(point, centers[i]);
        if (dist < min_dist) {
            min_dist = dist;
        }
    }
    return min_dist;
}

// Function to implement Farthest First Traversal
void farthest_first_traversal(Point* points, int n, int k, Point* centers) {
    // Initialize first center as the first point
    centers[0] = points[0];
    
    // For remaining k-1 centers
    for (int i = 1; i < k; i++) {
        double max_min_dist = -1;
        int farthest_point_idx = -1;
        
        // Find the point that maximizes the minimum distance to existing centers
        for (int j = 0; j < n; j++) {
            double min_dist = min_distance(points[j], centers, i);
            if (min_dist > max_min_dist) {
                max_min_dist = min_dist;
                farthest_point_idx = j;
            }
        }
        
        // Set the farthest point as the next center
        centers[i] = points[farthest_point_idx];
    }
}

int main() {
    int n, k;
    
    // Read number of points and number of centers
    scanf("%d %d", &n, &k);
    
    // Allocate memory for points and centers
    Point* points = (Point*)malloc(n * sizeof(Point));
    Point* centers = (Point*)malloc(k * sizeof(Point));
    
    // Read points
    for (int i = 0; i < n; i++) {
        scanf("%lf %lf", &points[i].x, &points[i].y);
    }
    
    // Perform Farthest First Traversal
    farthest_first_traversal(points, n, k, centers);
    
    // Print the centers
    for (int i = 0; i < k; i++) {
        printf("%.10g %.10g\n", centers[i].x, centers[i].y);
    }
    
    // Free allocated memory
    free(points);
    free(centers);
    
    return 0;
}
```

