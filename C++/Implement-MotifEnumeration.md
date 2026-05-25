```cpp
#include <iostream>
#include <vector>
#include <string>
#include <set>
#include <algorithm>

using namespace std;

// Function to generate all k-mers from a string
vector<string> get_kmers(const string& text, int k) {
    vector<string> kmers;
    for (int i = 0; i <= (int)text.length() - k; i++) {
        kmers.push_back(text.substr(i, k));
    }
    return kmers;
}

// Function to calculate Hamming distance between two strings
int hamming_distance(const string& s1, const string& s2) {
    int distance = 0;
    for (int i = 0; i < (int)s1.length(); i++) {
        if (s1[i] != s2[i]) {
            distance++;
        }
    }
    return distance;
}

// Function to generate all possible k-mers with up to d mismatches
vector<string> get_mismatches(const string& pattern, int d) {
    vector<string> result;
    if (d == 0) {
        result.push_back(pattern);
        return result;
    }
    
    // Generate all k-mers of length k
    vector<string> kmers = get_kmers(pattern, pattern.length());
    
    // For each k-mer, generate all possible mismatches
    for (const string& kmer : kmers) {
        // Generate all possible strings of length k with at most d mismatches
        for (int i = 0; i < (1 << pattern.length()); i++) {
            string candidate = pattern;
            int mismatches = 0;
            for (int j = 0; j < pattern.length(); j++) {
                if (i & (1 << j)) {
                    // Change the character at position j
                    if (candidate[j] == 'A') candidate[j] = 'C';
                    else if (candidate[j] == 'C') candidate[j] = 'G';
                    else if (candidate[j] == 'G') candidate[j] = 'T';
                    else if (candidate[j] == 'T') candidate[j] = 'A';
                    mismatches++;
                }
            }
            if (mismatches <= d) {
                result.push_back(candidate);
            }
        }
    }
    
    // Better approach: generate all k-mers of length k with at most d mismatches
    result.clear();
    if (pattern.length() == 0) return result;
    
    // Generate all k-mers of length k with up to d mismatches
    vector<string> all_kmers = {"A", "C", "G", "T"};
    vector<string> current = all_kmers;
    
    for (int i = 1; i < pattern.length(); i++) {
        vector<string> next;
        for (const string& s : current) {
            for (const string& base : all_kmers) {
                next.push_back(s + base);
            }
        }
        current = next;
    }
    
    // Check which of these k-mers have at most d mismatches with pattern
    for (const string& kmer : current) {
        if (hamming_distance(pattern, kmer) <= d) {
            result.push_back(kmer);
        }
    }
    
    return result;
}

// Function to generate all k-mers of length k
vector<string> generate_all_kmers(int k) {
    vector<string> result;
    if (k == 0) {
        result.push_back("");
        return result;
    }
    
    vector<string> prev = generate_all_kmers(k - 1);
    vector<string> bases = {"A", "C", "G", "T"};
    
    for (const string& base : bases) {
        for (const string& s : prev) {
            result.push_back(base + s);
        }
    }
    
    return result;
}

// Function to check if a pattern appears in text with at most d mismatches
bool appears_with_mismatch(const string& pattern, const string& text, int d) {
    int k = pattern.length();
    for (int i = 0; i <= (int)text.length() - k; i++) {
        string substring = text.substr(i, k);
        if (hamming_distance(pattern, substring) <= d) {
            return true;
        }
    }
    return false;
}

// Main function to solve motif enumeration
vector<string> motif_enumeration(const vector<string>& dna, int k, int d) {
    set<string> potential_motifs;
    set<string> final_motifs;
    
    // Get all k-mers from the first DNA string
    vector<string> first_dna_kmers = get_kmers(dna[0], k);
    
    // For each k-mer in the first DNA string
    for (const string& kmer : first_dna_kmers) {
        // Generate all k-mers with at most d mismatches
        vector<string> candidates = get_mismatches(kmer, d);
        
        for (const string& candidate : candidates) {
            bool found_in_all = true;
            
            // Check if this candidate appears in all DNA strings with at most d mismatches
            for (int i = 1; i < dna.size(); i++) {
                bool found_in_current = false;
                vector<string> current_kmers = get_kmers(dna[i], k);
                
                for (const string& current_kmer : current_kmers) {
                    if (hamming_distance(candidate, current_kmer) <= d) {
                        found_in_current = true;
                        break;
                    }
                }
                
                if (!found_in_current) {
                    found_in_all = false;
                    break;
                }
            }
            
            if (found_in_all) {
                final_motifs.insert(candidate);
            }
        }
    }
    
    // Convert set to vector
    vector<string> result(final_motifs.begin(), final_motifs.end());
    return result;
}

// Better approach for motif enumeration
vector<string> motif_enumeration_better(const vector<string>& dna, int k, int d) {
    set<string> motifs;
    
    // Generate all possible k-mers
    vector<string> all_kmers = generate_all_kmers(k);
    
    // For each k-mer, check if it's a valid motif
    for (const string& pattern : all_kmers) {
        bool found_in_all = true;
        
        // Check if pattern appears in each DNA string with at most d mismatches
        for (const string& sequence : dna) {
            bool found = false;
            for (int i = 0; i <= (int)sequence.length() - k; i++) {
                string substring = sequence.substr(i, k);
                if (hamming_distance(pattern, substring) <= d) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                found_in_all = false;
                break;
            }
        }
        
        if (found_in_all) {
            motifs.insert(pattern);
        }
    }
    
    vector<string> result(motifs.begin(), motifs.end());
    return result;
}

int main() {
    // Read input
    int k, d;
    cin >> k >> d;
    
    vector<string> dna;
    string line;
    while (cin >> line) {
        dna.push_back(line);
    }
    
    // Solve motif enumeration
    vector<string> result = motif_enumeration_better(dna, k, d);
    
    // Output results
    for (const string& motif : result) {
        cout << motif << " ";
    }
    cout << endl;
    
    return 0;
}
```

