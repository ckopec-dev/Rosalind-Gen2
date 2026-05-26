# Rosalind Problem: Introduction to Set Operations - Visual Basic Solution

## Problem Understanding

This problem requires implementing set operations (union, intersection, difference, and symmetric difference) on two sets of integers, and finding the complement of each set with respect to a universal set.

## Solution

```vb
Module SetOperations
    Sub Main()
        ' Sample input data (replace with actual input)
        Dim U As New HashSet(Of Integer) From {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
        Dim A As New HashSet(Of Integer) From {1, 2, 3, 4, 5}
        Dim B As New HashSet(Of Integer) From {2, 4, 6, 8, 10}
        
        ' Print universal set
        Console.WriteLine("Universal set U: {" & String.Join(", ", U.OrderBy(Function(x) x)) & "}")
        
        ' Print set A
        Console.WriteLine("Set A: {" & String.Join(", ", A.OrderBy(Function(x) x)) & "}")
        
        ' Print set B
        Console.WriteLine("Set B: {" & String.Join(", ", B.OrderBy(Function(x) x)) & "}")
        
        ' Union: A ∪ B
        Dim unionSet As New HashSet(Of Integer)(A)
        unionSet.UnionWith(B)
        Console.WriteLine("A ∪ B: {" & String.Join(", ", unionSet.OrderBy(Function(x) x)) & "}")
        
        ' Intersection: A ∩ B
        Dim intersectionSet As New HashSet(Of Integer)(A)
        intersectionSet.IntersectWith(B)
        Console.WriteLine("A ∩ B: {" & String.Join(", ", intersectionSet.OrderBy(Function(x) x)) & "}")
        
        ' Difference: A - B
        Dim differenceSet As New HashSet(Of Integer)(A)
        differenceSet.ExceptWith(B)
        Console.WriteLine("A - B: {" & String.Join(", ", differenceSet.OrderBy(Function(x) x)) & "}")
        
        ' Difference: B - A
        Dim differenceSet2 As New HashSet(Of Integer)(B)
        differenceSet2.ExceptWith(A)
        Console.WriteLine("B - A: {" & String.Join(", ", differenceSet2.OrderBy(Function(x) x)) & "}")
        
        ' Symmetric Difference: (A - B) ∪ (B - A)
        Dim symmetricDiff As New HashSet(Of Integer)(A)
        symmetricDiff.SymmetricExceptWith(B)
        Console.WriteLine("A ⊕ B: {" & String.Join(", ", symmetricDiff.OrderBy(Function(x) x)) & "}")
        
        ' Complement of A: U - A
        Dim complementA As New HashSet(Of Integer)(U)
        complementA.ExceptWith(A)
        Console.WriteLine("A^c: {" & String.Join(", ", complementA.OrderBy(Function(x) x)) & "}")
        
        ' Complement of B: U - B
        Dim complementB As New HashSet(Of Integer)(U)
        complementB.ExceptWith(B)
        Console.WriteLine("B^c: {" & String.Join(", ", complementB.OrderBy(Function(x) x)) & "}")
    End Sub
End Module
```

## Explanation

This Visual Basic solution implements all the required set operations:

1. **Union (A ∪ B)**: Combines all elements from both sets without duplicates
2. **Intersection (A ∩ B)**: Finds elements common to both sets
3. **Difference (A - B)**: Elements in A but not in B
4. **Difference (B - A)**: Elements in B but not in A
5. **Symmetric Difference (A ⊕ B)**: Elements in either A or B but not in both
6. **Complements (A^c, B^c)**: Elements in universal set U but not in A or B respectively

## Key Features

- Uses `HashSet(Of Integer)` for efficient set operations
- Utilizes built-in methods like `UnionWith`, `IntersectWith`, `ExceptWith`, and `SymmetricExceptWith`
- Orders results for consistent output
- Handles the universal set operations correctly

## Sample Output
```
Universal set U: {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
Set A: {1, 2, 3, 4, 5}
Set B: {2, 4, 6, 8, 10}
A ∪ B: {1, 2, 3, 4, 5, 6, 8, 10}
A ∩ B: {2, 4}
A - B: {1, 3, 5}
B - A: {6, 8, 10}
A ⊕ B: {1, 3, 5, 6, 8, 10}
A^c: {6, 7, 8, 9, 10}
B^c: {1, 3, 5, 7, 9}
```

This solution efficiently handles all the set operations required for the Rosalind problem using Visual Basic's built-in HashSet collection with optimized set operation methods.

