# Rosalind Problem: Implement_TrieMatching in C#

## Problem Description
Given a string `Text` and a collection of strings `Patterns`, return all starting positions in `Text` where a string from `Patterns` appears as a substring.

## Solution Approach
We'll implement a trie data structure to efficiently search for multiple patterns in the text. The key steps are:
1. Build a trie from all patterns
2. For each position in the text, traverse the trie to find matches
3. Record all starting positions where patterns are found

## Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class TrieMatching
{
    public class TrieNode
    {
        public Dictionary<char, TrieNode> Children;
        public bool IsEndOfPattern;
        public int PatternIndex;
        
        public TrieNode()
        {
            Children = new Dictionary<char, TrieNode>();
            IsEndOfPattern = false;
            PatternIndex = -1;
        }
    }
    
    public class Trie
    {
        public TrieNode Root;
        
        public Trie()
        {
            Root = new TrieNode();
        }
        
        public void Insert(string pattern, int patternIndex)
        {
            TrieNode current = Root;
            
            foreach (char c in pattern)
            {
                if (!current.Children.ContainsKey(c))
                {
                    current.Children[c] = new TrieNode();
                }
                current = current.Children[c];
            }
            
            current.IsEndOfPattern = true;
            current.PatternIndex = patternIndex;
        }
        
        public List<int> Search(string text)
        {
            List<int> positions = new List<int>();
            
            for (int i = 0; i < text.Length; i++)
            {
                TrieNode current = Root;
                int j = i;
                
                while (j < text.Length && current.Children.ContainsKey(text[j]))
                {
                    current = current.Children[text[j]];
                    if (current.IsEndOfPattern)
                    {
                        positions.Add(i);
                        break;
                    }
                    j++;
                }
            }
            
            return positions;
        }
    }
    
    public static List<int> Solve(string text, List<string> patterns)
    {
        Trie trie = new Trie();
        
        // Insert all patterns into the trie
        for (int i = 0; i < patterns.Count; i++)
        {
            trie.Insert(patterns[i], i);
        }
        
        // Search for patterns in text
        return trie.Search(text);
    }
    
    public static void Main(string[] args)
    {
        // Read input from file or console
        string input = File.ReadAllText("input.txt");
        string[] lines = input.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        
        string text = lines[0];
        List<string> patterns = new List<string>();
        
        for (int i = 1; i < lines.Length; i++)
        {
            patterns.Add(lines[i]);
        }
        
        List<int> result = Solve(text, patterns);
        
        Console.WriteLine(string.Join(" ", result));
    }
}
```

## Example Usage

For input:
```
AATCGGGTTCAATCGGGGT
ATCG
GGGT
```

The output would be:
```
1 4 11 15
```

## Explanation

1. **Trie Construction**: We build a trie where each path from root to leaf represents a pattern from the input list.

2. **Pattern Search**: For each position in the text, we start traversing the trie:
   - If we find a character that exists in the trie, we continue
   - If we reach a node where `IsEndOfPattern` is true, we've found a match
   - We record the starting position of the match

3. **Time Complexity**: O(|Text| × |Patterns| × |Longest Pattern|) in worst case, but typically much better due to trie pruning.

4. **Space Complexity**: O(|Patterns| × |Longest Pattern|) for storing the trie structure.

This solution efficiently handles multiple pattern matching by leveraging the trie data structure for fast prefix-based searches.

