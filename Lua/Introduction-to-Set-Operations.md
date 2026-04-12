# Rosalind Problem: Introduction to Set Operations - Lua Solution

## Problem Understanding

This problem asks us to perform basic set operations (union, intersection, difference) on two sets of integers and output the results in a specific format.

## Solution

```lua
-- Read input from stdin
local input = {}
for line in io.lines() do
    table.insert(input, line)
end

-- Parse the input
local n = tonumber(input[1])  -- Universal set size
local set1_str = input[2]     -- First set
local set2_str = input[3]     -- Second set

-- Convert string representations to sets
local function string_to_set(str)
    local set = {}
    local elements = {}
    -- Remove braces and split by comma
    str = str:sub(2, #str-1)  -- Remove outer braces
    for num in str:gmatch("%d+") do
        table.insert(elements, tonumber(num))
    end
    -- Convert to set (no duplicates)
    for _, num in ipairs(elements) do
        set[num] = true
    end
    return set
end

-- Convert strings to sets
local set1 = string_to_set(set1_str)
local set2 = string_to_set(set2_str)

-- Helper function to print set in required format
local function print_set(set)
    local elements = {}
    for num, _ in pairs(set) do
        table.insert(elements, num)
    end
    table.sort(elements)
    if #elements == 0 then
        io.write("{}")
    else
        io.write("{")
        for i, num in ipairs(elements) do
            if i > 1 then io.write(", ") end
            io.write(num)
        end
        io.write("}")
    end
    io.write("\n")
end

-- Perform set operations
-- Union: set1 ∪ set2
local union = {}
for num, _ in pairs(set1) do
    union[num] = true
end
for num, _ in pairs(set2) do
    union[num] = true
end

-- Intersection: set1 ∩ set2
local intersection = {}
for num, _ in pairs(set1) do
    if set2[num] then
        intersection[num] = true
    end
end

-- Difference: set1 - set2
local diff1 = {}
for num, _ in pairs(set1) do
    if not set2[num] then
        diff1[num] = true
    end
end

-- Difference: set2 - set1
local diff2 = {}
for num, _ in pairs(set2) do
    if not set1[num] then
        diff2[num] = true
    end
end

-- Complement of set1 in universal set (1 to n)
local complement1 = {}
for i = 1, n do
    if not set1[i] then
        complement1[i] = true
    end
end

-- Complement of set2 in universal set (1 to n)
local complement2 = {}
for i = 1, n do
    if not set2[i] then
        complement2[i] = true
    end
end

-- Output results
print_set(union)
print_set(intersection)
print_set(diff1)
print_set(diff2)
print_set(complement1)
print_set(complement2)
```

## Explanation

This solution:

1. **Reads input**: Gets the universal set size and two sets from standard input
2. **Parses sets**: Converts string representations like `{1, 2, 3}` into Lua tables representing sets
3. **Performs operations**:
   - **Union**: All elements in either set
   - **Intersection**: Elements in both sets
   - **Difference**: Elements in first set but not second
   - **Complement**: Elements in universal set but not in given set
4. **Formats output**: Prints sets in the required format with braces and comma separation

## Sample Input/Output

**Input:**
```
10
{1, 2, 3, 4, 5}
{2, 8, 5, 10}
```

**Output:**
```
{1, 2, 3, 4, 5, 8, 10}
{2, 5}
{1, 3, 4}
{8, 10}
{6, 7, 8, 9, 10}
{1, 3, 4, 6, 7, 9}
```

The solution handles the set operations correctly and outputs results in the required format with proper sorting and formatting.

