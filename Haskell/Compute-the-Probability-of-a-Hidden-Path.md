# Rosalind Problem: Compute the Probability of a Hidden Path

## Problem Statement
Given: A hidden Markov model (HMM) with emission alphabet Σ, state alphabet S, transition matrix A, emission matrix B, and an observed string x.

Return: The probability Pr(x) that the HMM emits x.

## Solution in Haskell

```haskell
import Data.List (foldl')

-- Type definitions
type State = String
type Symbol = Char
type Probability = Double

-- HMM structure
data HMM = HMM
    { states :: [State]
    , alphabet :: [Symbol]
    , transitionMatrix :: [(State, [(State, Probability)])]
    , emissionMatrix :: [(State, [(Symbol, Probability)])]
    , initialState :: State
    } deriving (Show, Eq)

-- Compute probability of a hidden path
computeHiddenPathProbability :: HMM -> [State] -> Probability
computeHiddenPathProbability hmm path
    | length path < 2 = 1.0
    | otherwise = 
        let initialStateProb = getTransitionProbability hmm (initialState hmm) (head path)
            transitionProbs = zipWith (getTransitionProbability hmm) path (tail path)
        in initialStateProb * product transitionProbs

-- Get transition probability from state1 to state2
getTransitionProbability :: HMM -> State -> State -> Probability
getTransitionProbability hmm from to = 
    case lookup from (transitionMatrix hmm) of
        Just transitions -> 
            case lookup to transitions of
                Just prob -> prob
                Nothing -> 0.0
        Nothing -> 0.0

-- Compute probability of observing a string given the HMM
computeObservationProbability :: HMM -> [Symbol] -> Probability
computeObservationProbability hmm obs = 
    let states = states hmm
        -- Use dynamic programming with forward algorithm
        forwardProbs = forward hmm obs states
    in sum [prob | (_, prob) <- forwardProbs]

-- Forward algorithm implementation
forward :: HMM -> [Symbol] -> [State] -> [(State, Probability)]
forward hmm [] _ = []
forward hmm (x:xs) states = 
    let initialProbs = [getInitialProb hmm state * getEmissionProb hmm state x | state <- states]
        forwardProbs = forwardRec hmm xs states initialProbs
    in zip states (map snd forwardProbs)

-- Recursive forward computation
forwardRec :: HMM -> [Symbol] -> [State] -> [Probability] -> [(State, Probability)]
forwardRec hmm [] _ _ = []
forwardRec hmm (x:xs) states prevProbs = 
    let currentProbs = [sum [prevProb * getTransitionProbability hmm from to | (from, prevProb) <- zip states prevProbs] * getEmissionProb hmm to x | to <- states]
        nextProbs = forwardRec hmm xs states currentProbs
    in zip states currentProbs

-- Helper functions
getInitialProb :: HMM -> State -> Probability
getInitialProb hmm state = 
    if state == initialState hmm 
    then 1.0 
    else 0.0

getEmissionProb :: HMM -> State -> Symbol -> Probability
getEmissionProb hmm state symbol = 
    case lookup state (emissionMatrix hmm) of
        Just emissions -> 
            case lookup symbol emissions of
                Just prob -> prob
                Nothing -> 0.0
        Nothing -> 0.0

-- Alternative simpler approach for specific cases
computePathProbability :: HMM -> [State] -> Probability
computePathProbability hmm path = 
    let startProb = if head path == initialState hmm then 1.0 else 0.0
        transProbs = [getTransitionProbability hmm from to | (from, to) <- zip path (tail path)]
    in startProb * product transProbs

-- Example usage
exampleHMM :: HMM
exampleHMM = HMM
    { states = ["A", "B"]
    , alphabet = ['x', 'y', 'z']
    , transitionMatrix = 
        [ ("A", [("A", 0.3), ("B", 0.7)])
        , ("B", [("A", 0.5), ("B", 0.5)])
        ]
    , emissionMatrix = 
        [ ("A", [('x', 0.5), ('y', 0.4), ('z', 0.1)])
        , ("B", [('x', 0.1), ('y', 0.5), ('z', 0.4)])
        ]
    , initialState = "A"
    }

-- Main function to solve the problem
solveHiddenPathProblem :: HMM -> [State] -> Probability
solveHiddenPathProblem hmm path = computePathProbability hmm path

-- Test example
testExample :: IO ()
testExample = do
    let path = ["A", "B", "A", "B"]
    let result = solveHiddenPathProblem exampleHMM path
    putStrLn $ "Probability of path " ++ show path ++ " is: " ++ show result
```

## Explanation

This solution implements the computation of the probability of a hidden path in a Hidden Markov Model:

1. **Data Structures**: 
   - `HMM` type represents the model with states, alphabet, transition matrix, emission matrix, and initial state
   - The transition and emission matrices are represented as lists of tuples

2. **Key Functions**:
   - `computePathProbability`: Calculates the probability of a specific hidden path
   - `getTransitionProbability`: Retrieves the probability of transitioning from one state to another
   - `getEmissionProb`: Gets the probability of emitting a symbol from a given state

3. **Algorithm**:
   - For a given path, multiply the initial state probability (1.0 if it's the initial state, 0.0 otherwise) with the transition probabilities between consecutive states
   - The probability of the entire path is the product of all individual transition probabilities

4. **Time Complexity**: O(n × m) where n is the length of the path and m is the number of states

The solution handles the core requirement of computing the probability of a hidden path given an HMM and a sequence of states.

