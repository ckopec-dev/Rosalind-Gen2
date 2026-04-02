# Rosalind Problem: Implement_GreedyMotifSearch

## Problem Description
Implement the greedy motif search algorithm to find the best motif in a set of DNA sequences.

## Solution

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <climits>

using namespace std;

// Function to calculate the score of a motif matrix
int Score(const vector<string>& motifs) {
    int score = 0;
    int k = motifs[0].length();
    int t = motifs.size();
    
    for (int i = 0; i < k; i++) {
        vector<int> count(4, 0); // A, C, G, T
        
        for (int j = 0; j < t; j++) {
            char nucleotide = motifs[j][i];
            if (nucleotide == 'A') count[0]++;
            else if (nucleotide == 'C') count[1]++;
            else if (nucleotide == 'G') count[2]++;
            else if (nucleotide == 'T') count[3]++;
        }
        
        int max_count = *max_element(count.begin(), count.end());
        score += (t - max_count);
    }
    
    return score;
}

// Function to calculate the profile of a motif matrix
vector<vector<double>> Profile(const vector<string>& motifs) {
    int k = motifs[0].length();
    int t = motifs.size();
    vector<vector<double>> profile(4, vector<double>(k, 0.0));
    
    for (int i = 0; i < k; i++) {
        vector<int> count(4, 0);
        
        for (int j = 0; j < t; j++) {
            char nucleotide = motifs[j][i];
            if (nucleotide == 'A') count[0]++;
            else if (nucleotide == 'C') count[1]++;
            else if (nucleotide == 'G') count[2]++;
            else if (nucleotide == 'T') count[3]++;
        }
        
        for (int n = 0; n < 4; n++) {
            profile[n][i] = (double)count[n] / t;
        }
    }
    
    return profile;
}

// Function to calculate the probability of a k-mer given a profile
double Probability(const string& kmer, const vector<vector<double>>& profile) {
    double prob = 1.0;
    int k = kmer.length();
    
    for (int i = 0; i < k; i++) {
        char nucleotide = kmer[i];
        int index;
        if (nucleotide == 'A') index = 0;
        else if (nucleotide == 'C') index = 1;
        else if (nucleotide == 'G') index = 2;
        else if (nucleotide == 'T') index = 3;
        
        prob *= profile[index][i];
    }
    
    return prob;
}

// Function to find the most probable k-mer in a string given a profile
string MostProbableKmer(const string& text, int k, const vector<vector<double>>& profile) {
    double max_prob = -1.0;
    string most_probable = text.substr(0, k);
    
    for (int i = 0; i <= (int)text.length() - k; i++) {
        string kmer = text.substr(i, k);
        double prob = Probability(kmer, profile);
        
        if (prob > max_prob) {
            max_prob = prob;
            most_probable = kmer;
        }
    }
    
    return most_probable;
}

// Function to find the consensus string from a motif matrix
string Consensus(const vector<string>& motifs) {
    int k = motifs[0].length();
    int t = motifs.size();
    string consensus = "";
    
    for (int i = 0; i < k; i++) {
        vector<int> count(4, 0);
        
        for (int j = 0; j < t; j++) {
            char nucleotide = motifs[j][i];
            if (nucleotide == 'A') count[0]++;
            else if (nucleotide == 'C') count[1]++;
            else if (nucleotide == 'G') count[2]++;
            else if (nucleotide == 'T') count[3]++;
        }
        
        int max_index = max_element(count.begin(), count.end()) - count.begin();
        char consensus_nucleotide;
        if (max_index == 0) consensus_nucleotide = 'A';
        else if (max_index == 1) consensus_nucleotide = 'C';
        else if (max_index == 2) consensus_nucleotide = 'G';
        else consensus_nucleotide = 'T';
        
        consensus += consensus_nucleotide;
    }
    
    return consensus;
}

// Main greedy motif search function
vector<string> GreedyMotifSearch(const vector<string>& dna, int k, int t) {
    int n = dna[0].length();
    vector<string> best_motifs;
    
    // Initialize best_motifs with first k-mers from each string
    for (int i = 0; i < t; i++) {
        best_motifs.push_back(dna[i].substr(0, k));
    }
    
    // Try all possible k-mers in the first string as the first motif
    for (int i = 0; i <= n - k; i++) {
        vector<string> motifs;
        motifs.push_back(dna[0].substr(i, k));
        
        // Build motifs for remaining strings
        for (int j = 1; j < t; j++) {
            vector<vector<double>> profile = Profile(motifs);
            string most_probable = MostProbableKmer(dna[j], k, profile);
            motifs.push_back(most_probable);
        }
        
        // Update best motifs if current motifs are better
        if (Score(motifs) < Score(best_motifs)) {
            best_motifs = motifs;
        }
    }
    
    return best_motifs;
}

int main() {
    // Example input
    vector<string> dna = {
        "GGCGTTCAGGCA",
        "AAGAATCAGTCA",
        "CAAGGAGTTCGC",
        "CACGTCAATCAC",
        "CAATAATATCAC"
    };
    
    int k = 3;
    int t = dna.size();
    
    vector<string> result = GreedyMotifSearch(dna, k, t);
    
    for (const string& motif : result) {
        cout << motif << " ";
    }
    cout << endl;
    
    return 0;
}
```

## Explanation

This implementation solves the Greedy Motif Search problem by:

1. **Score Function**: Calculates the score of a motif matrix by counting the number of mismatches from the consensus string.

2. **Profile Function**: Computes the profile matrix (probability matrix) for a given set of motifs.

3. **Probability Function**: Calculates the probability of a k-mer given a profile matrix.

4. **MostProbableKmer Function**: Finds the most probable k-mer in a string given a profile.

5. **GreedyMotifSearch Function**: 
   - Tries all possible k-mers in the first DNA string as the first motif
   - For each choice, builds a motif matrix by finding the most probable k-mer in each subsequent DNA string
   - Keeps track of the best scoring motif matrix found

## Time Complexity
- O(t * n * k) where t is the number of DNA strings, n is the length of each string, and k is the motif length

## Space Complexity
- O(t * k) for storing the motif matrix and profile matrix

The algorithm uses a greedy approach, making locally optimal choices at each step to find a globally good solution for motif finding.

