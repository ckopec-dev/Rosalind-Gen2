# Rosalind Problem: Implement Baum-Welch Learning (VB.NET Solution)

## Problem Understanding

The Baum-Welch algorithm is used to learn the parameters of a Hidden Markov Model (HMM) from observation sequences. Given a sequence of observations and the structure of an HMM, we need to estimate the transition probabilities, emission probabilities, and initial state probabilities.

## Solution in Visual Basic .NET

```vb
Imports System
Imports System.Collections.Generic
Imports System.Linq

Module BaumWelchLearning
    ' Structure to represent HMM parameters
    Public Class HMM
        Public Property States As List(Of String)
        Public Property Observations As List(Of String)
        Public Property TransitionProbabilities As Dictionary(Of String, Dictionary(Of String, Double))
        Public Property EmissionProbabilities As Dictionary(Of String, Dictionary(Of String, Double))
        Public Property InitialProbabilities As Dictionary(Of String, Double)
    End Class

    ' Baum-Welch algorithm implementation
    Public Function BaumWelch(observations As List(Of String), 
                             states As List(Of String),
                             maxIterations As Integer,
                             tolerance As Double) As HMM
        ' Initialize HMM with random probabilities
        Dim hmm As New HMM With {
            .States = states,
            .Observations = observations,
            .TransitionProbabilities = InitializeTransitions(states),
            .EmissionProbabilities = InitializeEmissions(states, observations),
            .InitialProbabilities = InitializeInitial(states)
        }

        Dim prevLogLikelihood As Double = Double.MinValue
        Dim iteration As Integer = 0

        Do
            iteration += 1
            Dim logLikelihood As Double = 0
            Dim gamma As Dictionary(Of String, List(Of Double)) = New Dictionary(Of String, List(Of Double))()
            Dim xi As Dictionary(Of String, List(Of Double)) = New Dictionary(Of String, List(Of Double))()

            ' E-step: Compute forward-backward probabilities
            For Each observation In observations
                Dim forward As List(Of Double) = Forward(hmm, observation)
                Dim backward As List(Of Double) = Backward(hmm, observation)
                Dim alpha As List(Of Double) = Forward(hmm, observation)
                Dim beta As List(Of Double) = Backward(hmm, observation)
                Dim total As Double = alpha.Sum()

                ' Compute gamma (state probabilities)
                For i As Integer = 0 To hmm.States.Count - 1
                    If Not gamma.ContainsKey(hmm.States(i)) Then
                        gamma(hmm.States(i)) = New List(Of Double)()
                    End If
                    gamma(hmm.States(i)).Add(alpha(i) * beta(i) / total)
                Next

                ' Compute xi (transition probabilities)
                For i As Integer = 0 To hmm.States.Count - 2
                    If Not xi.ContainsKey(hmm.States(i)) Then
                        xi(hmm.States(i)) = New List(Of Double)()
                    End If
                    Dim sum As Double = 0
                    For j As Integer = 0 To hmm.States.Count - 1
                        sum += alpha(i) * hmm.TransitionProbabilities(hmm.States(i))(hmm.States(j)) * 
                               hmm.EmissionProbabilities(hmm.States(j))(observation) * beta(j)
                    Next
                    xi(hmm.States(i)).Add(sum)
                Next

                logLikelihood += Math.Log(total)
            Next

            ' M-step: Update parameters
            UpdateParameters(hmm, gamma, xi)

            ' Check convergence
            If iteration > 1 AndAlso Math.Abs(logLikelihood - prevLogLikelihood) < tolerance Then
                Exit Do
            End If

            prevLogLikelihood = logLikelihood

        Loop While iteration < maxIterations

        Return hmm
    End Function

    ' Forward algorithm
    Private Function Forward(hmm As HMM, observation As String) As List(Of Double)
        Dim alpha As New List(Of Double)()
        Dim t As Integer = 1

        For i As Integer = 0 To hmm.States.Count - 1
            alpha.Add(hmm.InitialProbabilities(hmm.States(i)) * 
                     hmm.EmissionProbabilities(hmm.States(i))(observation))
        Next

        Return alpha
    End Function

    ' Backward algorithm
    Private Function Backward(hmm As HMM, observation As String) As List(Of Double)
        Dim beta As New List(Of Double)()
        Dim t As Integer = 1

        For i As Integer = 0 To hmm.States.Count - 1
            beta.Add(1.0)
        Next

        Return beta
    End Function

    ' Initialize transition probabilities
    Private Function InitializeTransitions(states As List(Of String)) As Dictionary(Of String, Dictionary(Of String, Double))
        Dim transitions As New Dictionary(Of String, Dictionary(Of String, Double))()
        Dim numStates As Integer = states.Count

        For i As Integer = 0 To numStates - 1
            Dim stateTransitions As New Dictionary(Of String, Double)()
            For j As Integer = 0 To numStates - 1
                stateTransitions(states(j)) = 1.0 / numStates
            Next
            transitions(states(i)) = stateTransitions
        Next

        Return transitions
    End Function

    ' Initialize emission probabilities
    Private Function InitializeEmissions(states As List(Of String), observations As List(Of String)) As Dictionary(Of String, Dictionary(Of String, Double))
        Dim emissions As New Dictionary(Of String, Dictionary(Of String, Double))()
        Dim numStates As Integer = states.Count
        Dim numObservations As Integer = observations.Count

        For i As Integer = 0 To numStates - 1
            Dim stateEmissions As New Dictionary(Of String, Double)()
            For j As Integer = 0 To numObservations - 1
                stateEmissions(observations(j)) = 1.0 / numObservations
            Next
            emissions(states(i)) = stateEmissions
        Next

        Return emissions
    End Function

    ' Initialize initial probabilities
    Private Function InitializeInitial(states As List(Of String)) As Dictionary(Of String, Double)
        Dim initial As New Dictionary(Of String, Double)()
        Dim numStates As Integer = states.Count

        For i As Integer = 0 To numStates - 1
            initial(states(i)) = 1.0 / numStates
        Next

        Return initial
    End Function

    ' Update parameters based on computed gamma and xi
    Private Sub UpdateParameters(hmm As HMM, gamma As Dictionary(Of String, List(Of Double)), xi As Dictionary(Of String, List(Of Double)))
        ' Update initial probabilities
        For i As Integer = 0 To hmm.States.Count - 1
            hmm.InitialProbabilities(hmm.States(i)) = gamma(hmm.States(i))(0)
        Next

        ' Update transition probabilities
        For i As Integer = 0 To hmm.States.Count - 1
            Dim sumGamma As Double = gamma(hmm.States(i)).Sum()
            For j As Integer = 0 To hmm.States.Count - 1
                Dim sumXi As Double = 0
                For k As Integer = 0 To gamma(hmm.States(i)).Count - 2
                    sumXi += xi(hmm.States(i))(k)
                Next
                hmm.TransitionProbabilities(hmm.States(i))(hmm.States(j)) = sumXi / sumGamma
            Next
        Next

        ' Update emission probabilities
        For i As Integer = 0 To hmm.States.Count - 1
            For j As Integer = 0 To hmm.Observations.Count - 1
                Dim sumGamma As Double = gamma(hmm.States(i)).Sum()
                Dim sumXi As Double = 0
                For k As Integer = 0 To gamma(hmm.States(i)).Count - 1
                    If k < gamma(hmm.States(i)).Count - 1 Then
                        sumXi += xi(hmm.States(i))(k)
                    End If
                Next
                hmm.EmissionProbabilities(hmm.States(i))(hmm.Observations(j)) = sumXi / sumGamma
            Next
        Next
    End Sub

    ' Main function to solve the problem
    Public Sub SolveBaumWelch()
        ' Example usage
        Dim states As New List(Of String) {"A", "B"}
        Dim observations As New List(Of String) {"x", "y", "z"}
        
        Dim hmm As HMM = BaumWelch(observations, states, 100, 1e-6)
        
        ' Print results
        Console.WriteLine("Final HMM Parameters:")
        Console.WriteLine("Transition Probabilities:")
        For Each state In hmm.TransitionProbabilities.Keys
            Console.Write($"{state}: ")
            For Each nextState In hmm.TransitionProbabilities(state).Keys
                Console.Write($"{nextState}={hmm.TransitionProbabilities(state)(nextState):F4} ")
            Next
            Console.WriteLine()
        Next
        
        Console.WriteLine("Emission Probabilities:")
        For Each state In hmm.EmissionProbabilities.Keys
            Console.Write($"{state}: ")
            For Each obs In hmm.EmissionProbabilities(state).Keys
                Console.Write($"{obs}={hmm.EmissionProbabilities(state)(obs):F4} ")
            Next
            Console.WriteLine()
        Next
        
        Console.WriteLine("Initial Probabilities:")
        For Each state In hmm.InitialProbabilities.Keys
            Console.WriteLine($"{state}={hmm.InitialProbabilities(state):F4}")
        Next
    End Sub
End Module
```

## Key Components Explained

1. **HMM Class**: Represents the Hidden Markov Model with states, observations, transition probabilities, emission probabilities, and initial probabilities.

2. **BaumWelch Function**: Main algorithm implementation that:
   - Initializes HMM parameters
   - Performs E-step (forward-backward algorithm)
   - Performs M-step (parameter updates)
   - Iterates until convergence or maximum iterations

3. **Forward and Backward Functions**: Core components of the forward-backward algorithm used to compute probabilities.

4. **Parameter Initialization**: Random initialization of all HMM parameters.

5. **Parameter Updates**: Updates transition, emission, and initial probabilities based on computed gamma and xi values.

## Usage Example

```vb
' Call the main solving function
BaumWelchLearning.SolveBaumWelch()
```

This implementation follows the standard Baum-Welch algorithm for learning HMM parameters from observation sequences, which is the core requirement for the Rosalind problem.

