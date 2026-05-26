# Rosalind Problem: Implement Viterbi Learning in C#

## Problem Description
Viterbi Learning is an algorithm for learning the parameters of a Hidden Markov Model (HMM) when we have both observations and hidden states. This is an extension of the Baum-Welch algorithm that also learns the transition and emission probabilities.

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class ViterbiLearning
{
    public static void Main()
    {
        // Example input (this would typically come from stdin)
        string[] lines = {
            "--------",
            "A B",
            "--------",
            "x y z",
            "--------",
            "0.5 0.5",
            "0.5 0.5",
            "0.5 0.5",
            "0.5 0.5",
            "0.5 0.5",
            "0.5 0.5",
            "0.5 0.5",
            "0.5 0.5",
            "--------",
            "A B A B A B A B A B",
            "--------"
        };

        var result = RunViterbiLearning(lines);
        Console.WriteLine(result);
    }

    public static string RunViterbiLearning(string[] input)
    {
        // Parse input
        var lines = input.ToList();
        int index = 0;
        
        // Skip separator lines
        while (index < lines.Count && lines[index] != "--------")
            index++;
        index++;
        
        // Parse states
        var states = lines[index++].Split(' ').ToList();
        int numStates = states.Count;
        
        // Skip separator
        while (index < lines.Count && lines[index] != "--------")
            index++;
        index++;
        
        // Parse emissions
        var emissions = lines[index++].Split(' ').ToList();
        int numEmissions = emissions.Count;
        
        // Skip separator
        while (index < lines.Count && lines[index] != "--------")
            index++;
        index++;
        
        // Parse initial probabilities
        var initialProbs = lines[index++].Split(' ').Select(double.Parse).ToList();
        
        // Parse transition matrix
        var transitionMatrix = new double[numStates, numStates];
        for (int i = 0; i < numStates; i++)
        {
            var row = lines[index++].Split(' ').Select(double.Parse).ToList();
            for (int j = 0; j < numStates; j++)
            {
                transitionMatrix[i, j] = row[j];
            }
        }
        
        // Parse emission matrix
        var emissionMatrix = new double[numStates, numEmissions];
        for (int i = 0; i < numStates; i++)
        {
            var row = lines[index++].Split(' ').Select(double.Parse).ToList();
            for (int j = 0; j < numEmissions; j++)
            {
                emissionMatrix[i, j] = row[j];
            }
        }
        
        // Skip separator
        while (index < lines.Count && lines[index] != "--------")
            index++;
        index++;
        
        // Parse observations
        var observations = lines[index++].Split(' ').ToList();
        
        // Run Viterbi Learning
        var result = ViterbiLearningAlgorithm(
            states, emissions, initialProbs, transitionMatrix, emissionMatrix, observations);
        
        return result;
    }

    public static string ViterbiLearningAlgorithm(
        List<string> states, List<string> emissions,
        List<double> initialProbs, double[,] transitionMatrix, double[,] emissionMatrix,
        List<string> observations)
    {
        int numStates = states.Count;
        int numEmissions = emissions.Count;
        int numObservations = observations.Count;
        
        // Convert observations to indices
        var emissionIndices = new List<int>();
        foreach (var obs in observations)
        {
            emissionIndices.Add(emissions.IndexOf(obs));
        }
        
        // Initialize parameters
        var newInitialProbs = new List<double>(initialProbs);
        var newTransitionMatrix = new double[numStates, numStates];
        var newEmissionMatrix = new double[numStates, numEmissions];
        
        // Copy initial matrices
        for (int i = 0; i < numStates; i++)
        {
            for (int j = 0; j < numStates; j++)
            {
                newTransitionMatrix[i, j] = transitionMatrix[i, j];
            }
            for (int j = 0; j < numEmissions; j++)
            {
                newEmissionMatrix[i, j] = emissionMatrix[i, j];
            }
        }
        
        // Viterbi Learning iterations
        for (int iteration = 0; iteration < 100; iteration++)
        {
            // E-step: Compute expected counts using Viterbi
            var expectedTransitions = new double[numStates, numStates];
            var expectedEmissions = new double[numStates, numEmissions];
            var expectedStates = new double[numStates];
            
            // For each sequence
            var viterbiPaths = new List<List<int>>();
            
            // Compute Viterbi path for the single sequence
            var path = ComputeViterbiPath(
                newInitialProbs, newTransitionMatrix, newEmissionMatrix,
                emissionIndices, numStates, numEmissions);
            viterbiPaths.Add(path);
            
            // Compute expected counts
            for (int t = 0; t < numObservations - 1; t++)
            {
                int state = path[t];
                int nextState = path[t + 1];
                int emission = emissionIndices[t];
                
                expectedTransitions[state, nextState]++;
                expectedEmissions[state, emission]++;
                expectedStates[state]++;
            }
            
            // Handle last state
            int lastState = path[numObservations - 1];
            int lastEmission = emissionIndices[numObservations - 1];
            expectedEmissions[lastState, lastEmission]++;
            expectedStates[lastState]++;
            
            // M-step: Update parameters
            // Update initial probabilities
            for (int i = 0; i < numStates; i++)
            {
                newInitialProbs[i] = expectedStates[i] / numObservations;
            }
            
            // Update transition probabilities
            for (int i = 0; i < numStates; i++)
            {
                double rowSum = expectedStates[i];
                if (rowSum > 0)
                {
                    for (int j = 0; j < numStates; j++)
                    {
                        newTransitionMatrix[i, j] = expectedTransitions[i, j] / rowSum;
                    }
                }
            }
            
            // Update emission probabilities
            for (int i = 0; i < numStates; i++)
            {
                double rowSum = expectedStates[i];
                if (rowSum > 0)
                {
                    for (int j = 0; j < numEmissions; j++)
                    {
                        newEmissionMatrix[i, j] = expectedEmissions[i, j] / rowSum;
                    }
                }
            }
        }
        
        // Format output
        return FormatOutput(states, emissions, newInitialProbs, newTransitionMatrix, newEmissionMatrix);
    }
    
    private static List<int> ComputeViterbiPath(
        List<double> initialProbs, double[,] transitionMatrix, double[,] emissionMatrix,
        List<int> observations, int numStates, int numEmissions)
    {
        int numObservations = observations.Count;
        var viterbi = new double[numStates, numObservations];
        var backpointers = new int[numStates, numObservations];
        
        // Initialize
        for (int i = 0; i < numStates; i++)
        {
            viterbi[i, 0] = initialProbs[i] * emissionMatrix[i, observations[0]];
            backpointers[i, 0] = 0;
        }
        
        // Recursion
        for (int t = 1; t < numObservations; t++)
        {
            for (int i = 0; i < numStates; i++)
            {
                double maxProb = 0;
                int maxState = 0;
                
                for (int j = 0; j < numStates; j++)
                {
                    double prob = viterbi[j, t - 1] * transitionMatrix[j, i];
                    if (prob > maxProb)
                    {
                        maxProb = prob;
                        maxState = j;
                    }
                }
                
                viterbi[i, t] = maxProb * emissionMatrix[i, observations[t]];
                backpointers[i, t] = maxState;
            }
        }
        
        // Backtrack
        var path = new List<int>();
        double maxFinalProb = 0;
        int finalState = 0;
        
        for (int i = 0; i < numStates; i++)
        {
            if (viterbi[i, numObservations - 1] > maxFinalProb)
            {
                maxFinalProb = viterbi[i, numObservations - 1];
                finalState = i;
            }
        }
        
        path.Add(finalState);
        int currentState = finalState;
        
        for (int t = numObservations - 1; t > 0; t--)
        {
            currentState = backpointers[currentState, t];
            path.Add(currentState);
        }
        
        path.Reverse();
        return path;
    }
    
    private static string FormatOutput(
        List<string> states, List<string> emissions,
        List<double> initialProbs, double[,] transitionMatrix, double[,] emissionMatrix)
    {
        var result = new List<string>();
        
        // Print initial probabilities
        result.Add("Initial probabilities:");
        result.Add(string.Join(" ", initialProbs.Select(p => p.ToString("F3"))));
        
        // Print transition matrix
        result.Add("Transition matrix:");
        for (int i = 0; i < states.Count; i++)
        {
            var row = new List<string>();
            for (int j = 0; j < states.Count; j++)
            {
                row.Add(transitionMatrix[i, j].ToString("F3"));
            }
            result.Add(string.Join(" ", row));
        }
        
        // Print emission matrix
        result.Add("Emission matrix:");
        for (int i = 0; i < states.Count; i++)
        {
            var row = new List<string>();
            for (int j = 0; j < emissions.Count; j++)
            {
                row.Add(emissionMatrix[i, j].ToString("F3"));
            }
            result.Add(string.Join(" ", row));
        }
        
        return string.Join("\n", result);
    }
}
```

## Explanation

This implementation of Viterbi Learning follows these key steps:

1. **Input Parsing**: Reads the HMM parameters and observation sequence from input
2. **Initialization**: Sets up the initial HMM parameters
3. **Viterbi Learning Loop**: 
   - Uses Viterbi algorithm to find the most likely hidden state sequence for the current parameters
   - Computes expected counts for transitions and emissions
   - Updates the HMM parameters using maximum likelihood estimation
4. **Output Formatting**: Returns the learned parameters in the required format

## Key Components

- **Viterbi Algorithm**: Finds the most likely sequence of hidden states given observations
- **Expectation-Maximization**: Updates parameters based on expected counts
- **Matrix Operations**: Handles the transition and emission probability matrices

## Time Complexity
- O(N × M × T) where N is the number of states, M is the number of emissions, and T is the length of the observation sequence
- The algorithm typically converges in a few iterations

## Space Complexity
- O(N × T) for storing Viterbi matrices and backpointers

This implementation handles the core requirements of Viterbi Learning for HMM parameter estimation.

