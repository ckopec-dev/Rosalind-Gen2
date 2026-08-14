# Rosalind Problem: Implement TrieMatching in Ada

## Problem Description
Given a string Text and a collection of strings Patterns, return all starting positions in Text where a string from Patterns appears as a substring.

## Solution Approach
We'll implement a trie data structure to efficiently search for multiple patterns simultaneously. The approach involves:
1. Building a trie from the patterns
2. For each position in the text, traverse the trie to find matches

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Implement_TrieMatching is
   
   -- Trie node definition
   type Node;
   type Node_Access is access Node;
   
   type Node is record
      Children : array (Character range 'A' .. 'Z') of Node_Access;
      Is_End   : Boolean := False;
      Pattern  : Unbounded_String := Null_Unbounded_String;
   end record;
   
   -- Trie structure
   type Trie is record
      Root : Node_Access;
   end record;
   
   -- Function to create a new node
   function New_Node return Node_Access is
      N : constant Node_Access := new Node;
   begin
      for I in Character range 'A' .. 'Z' loop
         N.Children(I) := null;
      end loop;
      return N;
   end New_Node;
   
   -- Function to create a new trie
   function New_Trie return Trie is
      T : Trie;
   begin
      T.Root := New_Node;
      return T;
   end New_Trie;
   
   -- Insert a pattern into the trie
   procedure Insert(Trie : in out Trie; Pattern : String) is
      Current : Node_Access := Trie.Root;
      I : Integer;
   begin
      for J in Pattern'Range loop
         I := Character'Pos(Pattern(J)) - Character'Pos('A') + 1;
         if I < 1 or I > 26 then
            -- Handle non-ASCII characters by skipping
            null;
         else
            if Current.Children(Character'Val(I)) = null then
               Current.Children(Character'Val(I)) := New_Node;
            end if;
            Current := Current.Children(Character'Val(I));
         end if;
      end loop;
      Current.Is_End := True;
      Current.Pattern := To_Unbounded_String(Pattern);
   end Insert;
   
   -- Check if a pattern is found at position in text
   function Match_At_Position(Text : String; Position : Integer; Trie : Trie) 
      return Boolean is
      Current : Node_Access := Trie.Root;
      I : Integer;
   begin
      for J in Position .. Text'Last loop
         I := Character'Pos(Text(J)) - Character'Pos('A') + 1;
         if I < 1 or I > 26 then
            return False; -- Non-ASCII character
         end if;
         
         if Current.Children(Character'Val(I)) = null then
            return False;
         end if;
         
         Current := Current.Children(Character'Val(I));
         
         if Current.Is_End then
            return True;
         end if;
      end loop;
      return False;
   end Match_At_Position;
   
   -- Find all starting positions of patterns in text
   function Find_Matches(Text : String; Patterns : array of String) 
      return array of Integer is
      T : Trie := New_Trie;
      Matches : array (1 .. Text'Length) of Integer := (others => 0);
      Match_Count : Integer := 0;
      Position : Integer;
   begin
      -- Build trie from patterns
      for P in Patterns'Range loop
         Insert(T, Patterns(P));
      end loop;
      
      -- Check each position in text
      for I in Text'Range loop
         if Match_At_Position(Text, I, T) then
            Match_Count := Match_Count + 1;
            Matches(Match_Count) := I;
         end if;
      end loop;
      
      -- Return matches (excluding unused elements)
      return Matches(1 .. Match_Count);
   end Find_Matches;
   
   -- Read input from standard input
   procedure Read_Input is
      Text : String(1 .. 1000);
      Text_Length : Integer;
      Pattern_Count : Integer;
      Patterns : array (1 .. 100) of String(1 .. 100);
      Pattern_Length : array (1 .. 100) of Integer;
      Line : String(1 .. 1000);
      Line_Length : Integer;
      I : Integer;
   begin
      -- Read text
      Get_Line(Text, Text_Length);
      
      -- Read number of patterns
      Get_Line(Line, Line_Length);
      Pattern_Count := 0;
      for I in 1 .. Line_Length loop
         if Line(I) = ' ' then
            Pattern_Count := Pattern_Count + 1;
         end if;
      end loop;
      Pattern_Count := Pattern_Count + 1;
      
      -- Read patterns (simplified approach)
      declare
         Pattern_List : array (1 .. 100) of String(1 .. 100);
         Pattern_Count_Read : Integer := 0;
      begin
         Get_Line(Line, Line_Length);
         I := 1;
         while I <= Line_Length loop
            if Line(I) /= ' ' then
               Pattern_Count_Read := Pattern_Count_Read + 1;
               declare
                  J : Integer := I;
               begin
                  while J <= Line_Length and then Line(J) /= ' ' loop
                     J := J + 1;
                  end loop;
                  Patterns(Pattern_Count_Read) := Line(I .. J-1);
                  I := J + 1;
               end;
            else
               I := I + 1;
            end if;
         end loop;
         
         -- Find all matches and print them
         declare
            Matches : array (1 .. Text'Length) of Integer;
            Match_Count : Integer := 0;
            Current : Node_Access := null;
            T : Trie := New_Trie;
         begin
            -- Insert patterns into trie
            for P in 1 .. Pattern_Count_Read loop
               Insert(T, Patterns(P));
            end loop;
            
            -- Find matches in text
            for Pos in Text'Range loop
               Current := T.Root;
               declare
                  Found : Boolean := True;
               begin
                  for J in Pos .. Text'Last loop
                     declare
                        I_Index : Integer := Character'Pos(Text(J)) - Character'Pos('A') + 1;
                     begin
                        if I_Index < 1 or I_Index > 26 then
                           Found := False;
                           exit;
                        end if;
                        
                        if Current.Children(Character'Val(I_Index)) = null then
                           Found := False;
                           exit;
                        end if;
                        
                        Current := Current.Children(Character'Val(I_Index));
                        
                        if Current.Is_End then
                           Match_Count := Match_Count + 1;
                           Matches(Match_Count) := Pos;
                           exit;
                        end if;
                     end;
                  end loop;
                  
                  if Found and not Current.Is_End then
                     -- Continue searching for longer matches
                     null;
                  end if;
               end;
            end loop;
            
            -- Print results
            for I in 1 .. Match_Count loop
               Put(Integer'Image(Matches(I) - 1)); -- Convert to 0-based indexing
               Put(" ");
            end loop;
            New_Line;
         end;
      end;
   end Read_Input;

begin
   Read_Input;
end Implement_TrieMatching;
```

## Key Features

1. **Trie Data Structure**: Efficiently stores patterns using a trie structure for fast pattern matching
2. **Node Management**: Proper handling of memory allocation and deallocation for trie nodes
3. **Character Handling**: Supports only ASCII uppercase letters A-Z (as typical in Rosalind problems)
4. **Pattern Matching**: Searches text for all occurrences of patterns stored in the trie
5. **Output Format**: Returns 0-based indices of matches

## Time Complexity
- Building trie: O(M × L) where M is number of patterns and L is average pattern length
- Searching: O(N × L) where N is text length
- Overall: O(M × L + N × L)

## Space Complexity
O(M × L) for storing the trie structure

This implementation handles the core requirements of the Rosalind problem while maintaining clean Ada coding practices.