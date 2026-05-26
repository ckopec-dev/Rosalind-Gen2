# Rosalind Problem: Phylogeny Comparison with Split Distance

## Problem Description

In this problem, we need to compute the split distance between two phylogenetic trees. The split distance is the number of splits (edges) that are present in one tree but not in the other.

## Solution Approach

1. Parse the Newick format trees
2. Convert trees to splits representation
3. Compute the split distance by counting symmetric differences

## Rust Implementation

```rust
use std::collections::HashSet;
use std::io;

#[derive(Debug, Clone)]
struct TreeNode {
    name: Option<String>,
    children: Vec<TreeNode>,
    parent: Option<usize>,
}

impl TreeNode {
    fn new(name: Option<String>) -> Self {
        TreeNode {
            name,
            children: Vec::new(),
            parent: None,
        }
    }
}

fn parse_newick(newick: &str) -> TreeNode {
    let mut tree = TreeNode::new(None);
    let mut stack = vec![&mut tree];
    let mut current = &mut tree;
    let mut i = 0;
    
    while i < newick.len() {
        match newick.chars().nth(i).unwrap() {
            '(' => {
                let new_node = TreeNode::new(None);
                current.children.push(new_node);
                let idx = current.children.len() - 1;
                current.children[idx].parent = Some(stack.len() - 1);
                stack.push(&mut current.children[idx]);
                current = &mut current.children[idx];
            }
            ')' => {
                stack.pop();
                if let Some(node) = stack.last() {
                    current = node;
                }
            }
            ',' => {
                // Move back to parent and create sibling
                stack.pop();
                let parent = stack.last_mut().unwrap();
                let new_node = TreeNode::new(None);
                parent.children.push(new_node);
                let idx = parent.children.len() - 1;
                parent.children[idx].parent = Some(stack.len() - 1);
                stack.push(&mut parent.children[idx]);
                current = &mut parent.children[idx];
            }
            ':' => {
                // Skip branch lengths
                while i < newick.len() && newick.chars().nth(i).unwrap() != ',' && 
                      newick.chars().nth(i).unwrap() != ')' && 
                      newick.chars().nth(i).unwrap() != '(' {
                    i += 1;
                }
                i -= 1; // Adjust for loop increment
            }
            _ => {
                // Handle leaf node names
                let mut name_start = i;
                while i < newick.len() && 
                      newick.chars().nth(i).unwrap() != ':' && 
                      newick.chars().nth(i).unwrap() != ',' && 
                      newick.chars().nth(i).unwrap() != ')' && 
                      newick.chars().nth(i).unwrap() != '(' {
                    i += 1;
                }
                let name = newick[name_start..i].to_string();
                current.name = Some(name);
                i -= 1; // Adjust for loop increment
            }
        }
        i += 1;
    }
    
    tree
}

fn get_splits(node: &TreeNode, splits: &mut HashSet<HashSet<String>>, all_leaves: &HashSet<String>) {
    if node.children.is_empty() {
        // Leaf node - return empty set
        return;
    }
    
    // Get all leaves in this subtree
    let mut leaves_in_subtree = HashSet::new();
    get_leaves_in_subtree(node, &mut leaves_in_subtree);
    
    // Create split: leaves in subtree vs leaves not in subtree
    let mut split1 = leaves_in_subtree.clone();
    let mut split2 = all_leaves.difference(&leaves_in_subtree).cloned().collect();
    
    // Ensure consistent representation (smaller set first)
    if split1.len() > split2.len() {
        std::mem::swap(&mut split1, &mut split2);
    }
    
    splits.insert(split1);
    
    // Recursively process children
    for child in &node.children {
        get_splits(child, splits, all_leaves);
    }
}

fn get_leaves_in_subtree(node: &TreeNode, leaves: &mut HashSet<String>) {
    if node.children.is_empty() {
        if let Some(name) = &node.name {
            leaves.insert(name.clone());
        }
    } else {
        for child in &node.children {
            get_leaves_in_subtree(child, leaves);
        }
    }
}

fn get_all_leaves(node: &TreeNode) -> HashSet<String> {
    let mut leaves = HashSet::new();
    get_leaves_in_subtree(node, &mut leaves);
    leaves
}

fn compute_split_distance(tree1_str: &str, tree2_str: &str) -> usize {
    // Parse both trees
    let tree1 = parse_newick(tree1_str);
    let tree2 = parse_newick(tree2_str);
    
    // Get all leaves
    let leaves1 = get_all_leaves(&tree1);
    let leaves2 = get_all_leaves(&tree2);
    
    // Ensure both trees have the same leaves
    assert_eq!(leaves1, leaves2, "Trees must have the same leaves");
    
    // Get splits for both trees
    let mut splits1 = HashSet::new();
    let mut splits2 = HashSet::new();
    
    get_splits(&tree1, &mut splits1, &leaves1);
    get_splits(&tree2, &mut splits2, &leaves2);
    
    // Compute symmetric difference
    let mut symmetric_diff = 0;
    
    // Count splits in tree1 but not in tree2
    for split in &splits1 {
        if !splits2.contains(split) {
            symmetric_diff += 1;
        }
    }
    
    // Count splits in tree2 but not in tree1
    for split in &splits2 {
        if !splits1.contains(split) {
            symmetric_diff += 1;
        }
    }
    
    symmetric_diff
}

fn main() {
    let input = io::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = input.trim().lines().collect();
    
    if lines.len() < 2 {
        panic!("Need at least 2 lines for two trees");
    }
    
    let tree1 = lines[0];
    let tree2 = lines[1];
    
    let distance = compute_split_distance(tree1, tree2);
    println!("{}", distance);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_split_distance() {
        let tree1 = "((a,b),(c,d));";
        let tree2 = "((a,c),(b,d));";
        let distance = compute_split_distance(tree1, tree2);
        assert_eq!(distance, 2);
    }
    
    #[test]
    fn test_identical_trees() {
        let tree1 = "((a,b),(c,d));";
        let tree2 = "((a,b),(c,d));";
        let distance = compute_split_distance(tree1, tree2);
        assert_eq!(distance, 0);
    }
}
```

## Explanation

This implementation:

1. **Parses Newick format trees**: The `parse_newick` function converts a Newick string representation into a tree structure with nodes and children.

2. **Extracts splits**: The `get_splits` function recursively traverses the tree to identify all splits (bipartitions) of the leaves.

3. **Computes split distance**: The `compute_split_distance` function compares the splits from both trees and counts the symmetric difference.

4. **Handles edge cases**: The code ensures both trees have the same leaves and properly handles leaf node names.

## Key Concepts

- **Split**: A bipartition of the leaf set in a phylogenetic tree
- **Split distance**: The number of splits present in one tree but not in the other
- **Symmetric difference**: The count of splits unique to each tree

## Time Complexity
- Parsing trees: O(n) where n is the number of nodes
- Computing splits: O(n²) in worst case
- Computing distance: O(s₁ + s₂) where s₁, s₂ are number of splits in each tree

## Space Complexity
- O(n) for tree storage
- O(s) for splits storage where s is the total number of splits

The solution handles the standard Newick format for phylogenetic trees and correctly computes the split distance between two trees.

