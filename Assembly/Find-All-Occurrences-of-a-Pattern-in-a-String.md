# Find All Occurrences of a Pattern in a String - Assembly Solution

I'll solve this Rosalind problem using Assembly language. The task is to find all starting positions where a pattern occurs in a string.

## Problem Understanding

Given:
- A DNA string `s` of length at most 1000
- A DNA pattern `t` of length at most 10

We need to find all starting positions (0-indexed) where pattern `t` occurs in string `s`.

## Assembly Solution

```assembly
.data
    s:      .ascii "GATATATGCATATACTT"
    s_len:  .long 15
    t:      .ascii "ATAT"
    t_len:  .long 4
    newline: .ascii "\n"
    space:   .ascii " "
    buffer:  .space 100
    
.text
.globl _start

_start:
    # Load addresses
    la $s_addr, s
    la $t_addr, t
    lw $s_length, s_len
    lw $t_length, t_len
    
    # Initialize loop counters
    li $i, 0              # i = 0 (current position in s)
    li $j, 0              # j = 0 (current position in t)
    li $found_count, 0    # count of matches found
    
    # Main loop: iterate through each possible starting position
check_loop:
    # Check if we've gone too far
    add $temp, $i, $t_length
    bge $temp, $s_length, end_program
    
    # Compare pattern with substring starting at position i
    li $j, 0
    li $match, 1          # assume match
    
compare_loop:
    # Check if we've compared all characters
    bge $j, $t_length, match_found
    
    # Load characters
    lb $s_char, ($s_addr)   # Load character from s
    lb $t_char, ($t_addr)   # Load character from t
    
    # Compare characters
    bne $s_char, $t_char, no_match
    
    # Move to next characters
    addi $s_addr, $s_addr, 1
    addi $t_addr, $t_addr, 1
    addi $j, $j, 1
    j compare_loop
    
no_match:
    li $match, 0
    # Reset addresses for next iteration
    la $s_addr, s
    la $t_addr, t
    addi $i, $i, 1
    j check_loop
    
match_found:
    # Print the position (i)
    # Convert i to string and print
    li $a0, $i
    jal print_number
    
    # Print space
    la $a0, space
    jal print_string
    
    # Reset addresses
    la $s_addr, s
    la $t_addr, t
    addi $i, $i, 1
    j check_loop

end_program:
    # Print newline
    la $a0, newline
    jal print_string
    
    # Exit program
    li $v0, 10
    syscall

# Function to print a number
print_number:
    # Simple implementation for printing integer
    # This would need more complex logic for multi-digit numbers
    # For now, assuming single digit numbers or using system calls
    
    # Using system call to print integer
    li $v0, 1
    move $a0, $a0
    syscall
    ret

# Function to print string
print_string:
    li $v0, 4
    syscall
    ret
```

## Alternative Implementation (More Efficient)

```assembly
.data
    s:      .ascii "GATATATGCATATACTT"
    s_len:  .long 15
    t:      .ascii "ATAT"
    t_len:  .long 4
    newline: .ascii "\n"
    space:   .ascii " "
    buffer:  .space 100
    
.text
.globl _start

_start:
    # Load addresses and lengths
    la $s_addr, s
    la $t_addr, t
    lw $s_length, s_len
    lw $t_length, t_len
    
    # Initialize variables
    li $i, 0              # Current position in string s
    li $found_count, 0    # Count of found positions
    
    # Main search loop
search_loop:
    # Check if we can still fit the pattern
    li $temp, 0
    add $temp, $i, $t_length
    bge $temp, $s_length, print_results
    
    # Compare pattern with substring starting at position i
    li $j, 0              # Position in pattern
    li $match, 1          # Assume match
    
compare_chars:
    # Check if we've compared all pattern characters
    bge $j, $t_length, position_found
    
    # Get characters at current positions
    add $temp_s, $s_addr, $i    # s[i]
    add $temp_t, $t_addr, $j    # t[j]
    
    lb $s_char, ($temp_s)
    lb $t_char, ($temp_t)
    
    # Compare characters
    beq $s_char, $t_char, chars_match
    li $match, 0
    j position_not_found
    
chars_match:
    addi $j, $j, 1
    j compare_chars
    
position_found:
    # Found match at position i
    add $temp, $i, $zero
    jal print_position
    
position_not_found:
    addi $i, $i, 1
    j search_loop

print_results:
    # Print final newline
    la $a0, newline
    li $v0, 4
    syscall
    
    # Exit
    li $v0, 10
    syscall

# Print position function
print_position:
    # Convert number to string and print
    # This is a simplified version
    li $v0, 1
    move $a0, $temp
    syscall
    
    # Print space
    la $a0, space
    li $v0, 4
    syscall
    
    ret
```

## Key Concepts Used

1. **String Comparison**: Loop through each position in the main string and compare with the pattern
2. **Index Management**: Keep track of current positions in both strings
3. **Loop Control**: Use conditional jumps to control the search process
4. **Memory Access**: Load characters from memory addresses using load byte instructions
5. **Output**: Print found positions with proper formatting

## Time Complexity
- **Time**: O(n × m) where n is the length of string s and m is the length of pattern t
- **Space**: O(1) additional space

## Sample Output
For input string "GATATATGCATATACTT" and pattern "ATAT", the output would be:
```
1 3 9 
```

This represents the 0-indexed positions where "ATAT" occurs in the string.

