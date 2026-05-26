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

// Function to calculate the profile matrix with pseudocounts
vector<vector<double>> Profile(const vector<string>& motifs) {
    int k = motifs[0].length();
    int t = motifs.size();
    vector<vector<double>> profile(4, vector<double>(k, 1.0)); // Add pseudocounts
    
    for (int i = 0; i < k; i++) {
        for (int j = 0; j < t; j++) {
            char nucleotide = motifs[j][i];
            if (nucleotide == 'A') profile[0][i]++;
            else if (nucleotide == 'C') profile[1][i]++;
            else if (nucleotide == 'G') profile[2][i]++;
            else if (nucleotide == 'T') profile[3][i]++;
        }
    }
    
    // Normalize by total count (t + 4 for pseudocounts)
    for (int i = 0; i < k; i++) {
        for (int j = 0; j < 4; j++) {
            profile[j][i] /= (t + 4);
        }
    }
    
    return profile;
}

// Function to find the most probable k-mer in a string given a profile
string MostProbableKmer(const string& text, int k, const vector<vector<double>>& profile) {
    double max_prob = -1.0;
    string most_probable = text.substr(0, k);
    
    for (int i = 0; i <= (int)text.length() - k; i++) {
        string kmer = text.substr(i, k);
        double prob = 1.0;
        
        for (int j = 0; j < k; j++) {
            char nucleotide = kmer[j];
            int index;
            if (nucleotide == 'A') index = 0;
            else if (nucleotide == 'C') index = 1;
            else if (nucleotide == 'G') index = 2;
            else if (nucleotide == 'T') index = 3;
            
            prob *= profile[index][j];
        }
        
        if (prob > max_prob) {
            max_prob = prob;
            most_probable = kmer;
        }
    }
    
    return most_probable;
}

// Function to implement Greedy Motif Search with Pseudocounts
vector<string> GreedyMotifSearch(const vector<string>& dna, int k, int t) {
    vector<string> best_motifs;
    
    // Initialize with first k-mers from each string
    for (int i = 0; i < t; i++) {
        best_motifs.push_back(dna[i].substr(0, k));
    }
    
    // Try all possible k-mers in the first string as the first motif
    for (int i = 0; i <= (int) dna[0].length() - k; i++) {
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
        "TTGGGAATCTCT"
    };
    
    int k = 3;
    int t = 5;
    
    vector<string> result = GreedyMotifSearch(dna, k, t);
    
    for (const string& motif : result) {
        cout << motif << endl;
    }
    
    return 0;
}
```

