# Rosalind Problem: Compute the Probability of a Hidden Path

## Problem Description
Given: A hidden path π = π₁π₂…πₙ on a Markov chain with m states, where each state is labeled 1, 2, ..., m.

Return: The probability of the path π.

## Solution in Assembly Language

```assembly
.data
    # Path string (example: "12345")
    path: .ascii "12345"
    path_len: .long 5
    
    # Transition matrix (3x3 example)
    transition_matrix: .long 0.5, 0.3, 0.2
                       .long 0.2, 0.6, 0.2
                       .long 0.1, 0.3, 0.6
    
    # Initial probabilities (for first state)
    initial_probs: .long 1.0, 0.0, 0.0
    
    # Result storage
    probability: .double 1.0

.text
.globl _start

_start:
    # Initialize registers
    movl path_len(%esp), %ecx        # Load path length
    movl $0, %edx                    # Index counter
    movl $1, %esi                    # Start with probability = 1
    
    # Get first state
    movb path(,%edx,1), %al          # Load first character
    subb $'0', %al                   # Convert to integer (0-9)
    movl %eax, %ebx                  # Store first state
    
    # Initialize probability with initial probability of first state
    movl initial_probs(,%ebx,4), %eax
    movl %eax, %esi                  # Set probability
    
    # Loop through remaining states
loop:
    # Check if we've processed all states
    cmpl $1, %ecx                    # Compare length with 1
    jle end_loop                     # If <= 1, exit loop
    
    # Get current state (next in path)
    incl %edx                        # Increment index
    movb path(,%edx,1), %al          # Load character
    subb $'0', %al                   # Convert to integer
    movl %eax, %edi                  # Store current state
    
    # Get previous state
    decl %edx                        # Go back one position
    movb path(,%edx,1), %al          # Load character
    subb $'0', %al                   # Convert to integer
    movl %eax, %ebx                  # Store previous state
    
    # Calculate transition probability
    # Matrix[i][j] = transition_matrix[j + i*3]
    movl %ebx, %eax                  # Previous state index
    imull $3, %eax                   # Multiply by 3 for row offset
    addl %edi, %eax                  # Add column index
    movl transition_matrix(,%eax,4), %eax
    
    # Multiply current probability with transition probability
    imull %esi, %eax                 # Multiply with existing probability
    
    # Update probability
    movl %eax, %esi                  # Store updated probability
    
    # Decrement counter and continue loop
    decl %ecx                        # Decrement path length
    jmp loop                         # Continue to next state

end_loop:
    # Store final result
    movl %esi, probability(%esp)
    
    # Exit program
    movl $1, %eax                    # sys_exit
    movl $0, %ebx                    # exit status
    int $0x80                        # system call

# Alternative implementation using floating point operations
_floating_point_version:
    # Initialize with first state probability
    movl initial_probs(,%ebx,4), %eax
    cvttss2si %eax, %esi             # Convert to integer
    
    # Process each transition
    movl path_len(%esp), %ecx        # Path length
    movl $1, %edx                    # Start from second state
    
loop_fp:
    cmpl %ecx, %edx                  # Compare with length
    jge end_loop_fp                  # If >= length, exit
    
    # Get current and previous states
    movb path(,%edx,1), %al          # Current character
    subb $'0', %al                   # Convert to integer
    movl %eax, %edi                  # Current state
    
    decl %edx                        # Previous index
    movb path(,%edx,1), %al          # Previous character
    subb $'0', %al                   # Convert to integer
    movl %eax, %ebx                  # Previous state
    
    # Calculate transition probability using matrix
    movl %ebx, %eax                  # Previous state
    imull $3, %eax                   # Row offset
    addl %edi, %eax                  # Column offset
    movss transition_matrix(,%eax,4), %xmm0  # Load transition prob
    
    # Multiply with current probability
    movss probability(%esp), %xmm1   # Load current probability
    mulss %xmm0, %xmm1               # Multiply
    
    # Store result back
    movss %xmm1, probability(%esp)
    
    incl %edx                        # Next state
    jmp loop_fp                      # Continue

end_loop_fp:
    # Exit program
    movl $1, %eax                    # sys_exit
    movl $0, %ebx                    # exit status
    int $0x80                        # system call
```

## Key Concepts:

1. **State Representation**: Each state is represented as an integer (1, 2, ..., m)
2. **Transition Matrix**: Stored as a 1D array representing a 2D matrix
3. **Path Processing**: Iterate through consecutive pairs of states
4. **Probability Calculation**: Multiply transition probabilities along the path

## Algorithm Steps:
1. Initialize probability to 1.0
2. Get first state's initial probability
3. For each consecutive pair of states in the path:
   - Look up transition probability from previous to current state
   - Multiply this with existing probability
4. Return final accumulated probability

## Notes:
- This assembly code assumes a specific memory layout and calling convention
- Actual implementation would need proper initialization and error handling
- The example uses a 3-state Markov chain for demonstration purposes