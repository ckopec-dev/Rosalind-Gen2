```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#define MAX_STRINGS 1000
#define MAX_LENGTH 1000
#define ALPHABET_SIZE 4

char DNA[MAX_STRINGS][MAX_LENGTH];
int num_strings, string_length;
char alphabet[4] = {'A', 'C', 'G', 'T'};

// Function to calculate profile matrix
void calculate_profile(int motif_positions[], double profile[4][MAX_LENGTH]) {
    int i, j;
    int count[4][MAX_LENGTH] = {0};
    
    // Count nucleotides at each position
    for (i = 0; i < num_strings; i++) {
        for (j = 0; j < string_length; j++) {
            int nucleotide_index = -1;
            switch (DNA[i][j]) {
                case 'A': nucleotide_index = 0; break;
                case 'C': nucleotide_index = 1; break;
                case 'G': nucleotide_index = 2; break;
                case 'T': nucleotide_index = 3; break;
            }
            if (nucleotide_index != -1) {
                count[nucleotide_index][j]++;
            }
        }
    }
    
    // Calculate profile probabilities
    for (j = 0; j < string_length; j++) {
        int total = 0;
        for (i = 0; i < 4; i++) {
            total += count[i][j];
        }
        for (i = 0; i < 4; i++) {
            profile[i][j] = (double)count[i][j] / (total + 4); // Add pseudocounts
        }
    }
}

// Function to calculate probability of a k-mer given profile
double calculate_kmer_probability(char kmer[], double profile[4][MAX_LENGTH]) {
    double probability = 1.0;
    int i, j;
    
    for (j = 0; j < string_length; j++) {
        int nucleotide_index = -1;
        switch (kmer[j]) {
            case 'A': nucleotide_index = 0; break;
            case 'C': nucleotide_index = 1; break;
            case 'G': nucleotide_index = 2; break;
            case 'T': nucleotide_index = 3; break;
        }
        if (nucleotide_index != -1) {
            probability *= profile[nucleotide_index][j];
        }
    }
    
    return probability;
}

// Function to generate random k-mer from a string
void generate_random_kmer(char* string, int k, char* kmer) {
    int start = rand() % (strlen(string) - k + 1);
    strncpy(kmer, string + start, k);
    kmer[k] = '\0';
}

// Function to get k-mer at specific position in a string
void get_kmer_at_position(char* string, int position, int k, char* kmer) {
    strncpy(kmer, string + position, k);
    kmer[k] = '\0';
}

// Function to find best motif position in a string given profile
int find_best_position(char* string, double profile[4][MAX_LENGTH], int k) {
    int best_position = 0;
    double best_probability = -1.0;
    
    for (int i = 0; i <= (int)strlen(string) - k; i++) {
        char kmer[100];
        strncpy(kmer, string + i, k);
        kmer[k] = '\0';
        
        double prob = calculate_kmer_probability(kmer, profile);
        if (prob > best_probability) {
            best_probability = prob;
            best_position = i;
        }
    }
    
    return best_position;
}

// Function to calculate score of a motif matrix
int calculate_score(int motif_positions[]) {
    int i, j;
    int count[4][MAX_LENGTH] = {0};
    int score = 0;
    
    // Count nucleotides at each position
    for (i = 0; i < num_strings; i++) {
        for (j = 0; j < string_length; j++) {
            int nucleotide_index = -1;
            switch (DNA[i][j]) {
                case 'A': nucleotide_index = 0; break;
                case 'C': nucleotide_index = 1; break;
                case 'G': nucleotide_index = 2; break;
                case 'T': nucleotide_index = 3; break;
            }
            if (nucleotide_index != -1) {
                count[nucleotide_index][j]++;
            }
        }
    }
    
    // Calculate score
    for (j = 0; j < string_length; j++) {
        int max_count = 0;
        for (i = 0; i < 4; i++) {
            if (count[i][j] > max_count) {
                max_count = count[i][j];
            }
        }
        score += (num_strings - max_count);
    }
    
    return score;
}

// Gibbs Sampler implementation
void gibbs_sampler(int k, int num_iterations, int* best_motif_positions) {
    int i, j;
    int motif_positions[MAX_STRINGS];
    int best_score = INT_MAX;
    
    // Initialize random seed
    srand(time(NULL));
    
    // Initialize random motifs
    for (i = 0; i < num_strings; i++) {
        motif_positions[i] = rand() % (string_length - k + 1);
    }
    
    // Main Gibbs sampling loop
    for (int iter = 0; iter < num_iterations; iter++) {
        // Choose a random string
        int random_string = rand() % num_strings;
        
        // Remove the motif from the random string
        int current_position = motif_positions[random_string];
        
        // Calculate profile excluding the random string
        double profile[4][MAX_LENGTH] = {0};
        int temp_positions[MAX_STRINGS];
        for (i = 0; i < num_strings; i++) {
            temp_positions[i] = motif_positions[i];
        }
        temp_positions[random_string] = -1; // Mark as removed
        
        // Recalculate profile
        int count[4][MAX_LENGTH] = {0};
        int total_count[MAX_LENGTH] = {0};
        
        for (i = 0; i < num_strings; i++) {
            if (i != random_string) {
                for (j = 0; j < string_length; j++) {
                    int nucleotide_index = -1;
                    switch (DNA[i][j]) {
                        case 'A': nucleotide_index = 0; break;
                        case 'C': nucleotide_index = 1; break;
                        case 'G': nucleotide_index = 2; break;
                        case 'T': nucleotide_index = 3; break;
                    }
                    if (nucleotide_index != -1) {
                        count[nucleotide_index][j]++;
                        total_count[j]++;
                    }
                }
            }
        }
        
        // Calculate profile with pseudocounts
        for (j = 0; j < string_length; j++) {
            for (i = 0; i < 4; i++) {
                profile[i][j] = (double)(count[i][j] + 1) / (total_count[j] + 4);
            }
        }
        
        // Find best motif position for the random string
        int best_pos = find_best_position(DNA[random_string], profile, k);
        motif_positions[random_string] = best_pos;
        
        // Check if this is the best solution so far
        int current_score = calculate_score(motif_positions);
        if (current_score < best_score) {
            best_score = current_score;
            for (i = 0; i < num_strings; i++) {
                best_motif_positions[i] = motif_positions[i];
            }
        }
    }
}

int main() {
    int k, num_iterations;
    
    // Read input
    scanf("%d %d", &k, &num_iterations);
    scanf("%d", &num_strings);
    
    for (int i = 0; i < num_strings; i++) {
        scanf("%s", DNA[i]);
        string_length = strlen(DNA[i]);
    }
    
    // Run Gibbs sampler
    int best_motif_positions[MAX_STRINGS];
    gibbs_sampler(k, num_iterations, best_motif_positions);
    
    // Output results
    for (int i = 0; i < num_strings; i++) {
        char kmer[100];
        get_kmer_at_position(DNA[i], best_motif_positions[i], k, kmer);
        printf("%s\n", kmer);
    }
    
    return 0;
}
```

