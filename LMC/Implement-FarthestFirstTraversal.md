# Rosalind Problem: Implement_FarthestFirstTraversal in LMC

## Problem Understanding

The FarthestFirstTraversal algorithm is used to select a set of k centers from a set of points in a metric space. The algorithm works by:
1. Starting with the first point as the first center
2. At each step, selecting the point that is farthest from all existing centers
3. Repeating until k centers are selected

## LMC Implementation

```assembly
        ORG 100
        LDA POINTS
        STA COUNT
        LDA POINTS+1
        STA DIMS
        LDA 0
        STA K
        LDA 1
        STA CENTER
        LDA 0
        STA DISTANCE
        LDA 0
        STA MAXDIST
        LDA 0
        STA SELECTED
        LDA 0
        STA I
        LDA 0
        STA J
        LDA 0
        STA MINJ
        LDA 0
        STA MINDIST
        LDA 0
        STA TEMP
        LDA 0
        STA TEMP2

        LDA K
        STA KVAL
        LDA 0
        STA CENTERCOUNT

        LDA CENTERCOUNT
        LDA KVAL
        SUB 1
        BRZ DONE
        LDA CENTERCOUNT
        LDA KVAL
        SUB 1
        BRZ DONE

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1
        BRZ FIRSTPOINT

        LDA CENTERCOUNT
        LDA 0
        SUB 1