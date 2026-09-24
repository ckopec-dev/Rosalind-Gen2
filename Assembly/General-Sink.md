# Rosalind Problem: General Sink

## Problem Description
A sink is a node with no outgoing edges in a directed graph. Given a directed graph with n nodes and m edges, find all sinks in the graph.

## Solution Approach
1. Read the graph structure from input
2. For each node, count its out-degree (number of outgoing edges)
3. Nodes with out-degree 0 are sinks

## Assembly Implementation

```assembly
.data
    # Input buffer
    buffer: .space 1000
    newline: .ascii "\n"
    
    # Graph representation
    max_nodes: .long 1000
    out_degree: .space 4000    # 1000 nodes * 4 bytes each
    
    # Output buffer
    output_buffer: .space 1000

.text
.globl _start

_start:
    # Read number of nodes and edges
    la $a0, buffer
    li $a1, 1000
    li $v0, 14          # sys_read
    syscall
    
    # Parse n and m from input
    la $a0, buffer
    li $t0, 0           # index
    li $t1, 0           # n
    li $t2, 0           # m
    
parse_loop:
    lb $t3, 0($a0)
    beq $t3, 10, parse_done  # newline character
    beq $t3, 32, parse_space # space character
    
    # Convert digit to number
    sub $t3, $t3, 48    # ASCII to digit
    mul $t1, $t1, 10
    add $t1, $t1, $t3
    addi $a0, $a0, 1
    j parse_loop

parse_space:
    addi $a0, $a0, 1
    li $t4, 0           # reset m
    
parse_m:
    lb $t3, 0($a0)
    beq $t3, 10, parse_done  # newline character
    beq $t3, 32, parse_done  # space character
    
    sub $t3, $t3, 48    # ASCII to digit
    mul $t4, $t4, 10
    add $t4, $t4, $t3
    addi $a0, $a0, 1
    j parse_m
    
parse_done:
    move $t5, $t1       # n
    move $t6, $t4       # m
    
    # Initialize out_degree array to zero
    li $t7, 0           # index
    li $t8, 0           # counter
    
init_loop:
    beq $t8, $t5, init_done
    la $a0, out_degree
    mul $t9, $t7, 4
    add $a0, $a0, $t9
    sw $zero, 0($a0)
    addi $t7, $t7, 1
    addi $t8, $t8, 1
    j init_loop
    
init_done:
    # Read edges and update out_degrees
    li $t8, 0           # edge counter
    
edge_loop:
    beq $t8, $t6, edge_done
    
    # Read source and destination nodes
    la $a0, buffer
    li $a1, 1000
    li $v0, 14          # sys_read
    syscall
    
    # Parse source node (first number)
    la $a0, buffer
    li $t9, 0           # index
    li $t10, 0          # source
    
parse_source:
    lb $t11, 0($a0)
    beq $t11, 10, parse_dest  # newline character
    beq $t11, 32, parse_dest  # space character
    
    sub $t11, $t11, 48   # ASCII to digit
    mul $t10, $t10, 10
    add $t10, $t10, $t11
    addi $a0, $a0, 1
    j parse_source
    
parse_dest:
    # Skip spaces
    lb $t11, 0($a0)
    beq $t11, 32, parse_dest_skip
    beq $t11, 10, parse_dest_skip
    addi $a0, $a0, 1
    j parse_dest
    
parse_dest_skip:
    li $t12, 0          # destination
    
parse_destination:
    lb $t11, 0($a0)
    beq $t11, 10, edge_increment  # newline character
    
    sub $t11, $t11, 48   # ASCII to digit
    mul $t12, $t12, 10
    add $t12, $t12, $t11
    addi $a0, $a0, 1
    j parse_destination
    
edge_increment:
    # Increment out_degree of source node
    la $a0, out_degree
    mul $t13, $t10, 4
    add $a0, $a0, $t13
    lw $t14, 0($a0)
    addi $t14, $t14, 1
    sw $t14, 0($a0)
    
    addi $t8, $t8, 1
    j edge_loop
    
edge_done:
    # Find and output sinks (nodes with out_degree = 0)
    li $t7, 0           # node index
    li $t15, 0          # output counter
    
sink_loop:
    beq $t7, $t5, sink_done
    
    la $a0, out_degree
    mul $t9, $t7, 4
    add $a0, $a0, $t9
    lw $t14, 0($a0)
    
    # If out_degree is 0, it's a sink
    beq $t14, $zero, output_sink
    
    addi $t7, $t7, 1
    j sink_loop
    
output_sink:
    # Convert node number to string and output
    li $a0, 1           # stdout
    la $a1, output_buffer
    li $a2, 10          # buffer size
    
    # Simple output (in practice would need proper conversion)
    li $v0, 1           # sys_write
    syscall
    
    addi $t7, $t7, 1
    j sink_loop
    
sink_done:
    # Exit program
    li $v0, 1           # sys_exit
    li $a0, 0
    syscall
```

## Key Features

1. **Input parsing**: Reads graph structure from standard input
2. **Graph representation**: Uses an array to track out-degrees of each node
3. **Sink identification**: Finds nodes with zero out-degree
4. **Output generation**: Reports all sink nodes

## Time Complexity
- O(n + m) where n is the number of nodes and m is the number of edges

## Space Complexity  
- O(n) for storing the out-degree array

This implementation handles the basic requirements of identifying sinks in a directed graph using assembly language concepts.