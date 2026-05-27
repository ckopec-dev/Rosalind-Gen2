# Rosalind Problem: Implement_LeaderboardCyclopeptideSequencing in Ada

Here's the solution for the Leaderboard Cyclopeptide Sequencing problem in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure Leaderboard_Cyclopeptide_Sequencing is
   
   -- Constants
   N := 20;  -- Size of leaderboard
   Mass_Table : array (Character) of Integer := 
     ('G' => 57, 'A' => 71, 'S' => 87, 'P' => 97, 'V' => 99,
      'T' => 101, 'C' => 103, 'I' => 113, 'L' => 113, 'N' => 114,
      'D' => 115, 'K' => 128, 'Q' => 128, 'E' => 129, 'M' => 131,
      'H' => 137, 'F' => 147, 'R' => 156, 'Y' => 163, 'W' => 186);
   
   -- Type definitions
   type Peptide is array (Positive range <>) of Character;
   type Peptide_Vector is array (Positive range <>) of Peptide;
   type Integer_Vector is array (Positive range <>) of Integer;
   
   -- Function to calculate theoretical spectrum of a peptide
   function Linear_Spectrum(Peptide : Peptide) return Integer_Vector is
      Length : constant Integer := Peptide'Length;
      Spectrum : Integer_Vector(1..Length * (Length + 1) / 2);
      Index : Integer := 1;
   begin
      Spectrum(1) := 0;
      Index := 2;
      
      for I in 1..Length loop
         declare
            Mass : Integer := 0;
         begin
            for J in I..Length loop
               Mass := Mass + Mass_Table(Peptide(J));
               Spectrum(Index) := Mass;
               Index := Index + 1;
            end loop;
         end;
      end loop;
      
      return Spectrum(1..Index-1);
   end Linear_Spectrum;
   
   -- Function to calculate cyclic spectrum of a peptide
   function Cyclic_Spectrum(Peptide : Peptide) return Integer_Vector is
      Length : constant Integer := Peptide'Length;
      Spectrum : Integer_Vector(1..Length * (Length - 1));
      Index : Integer := 1;
   begin
      Spectrum(1) := 0;
      Index := 2;
      
      for I in 1..Length loop
         declare
            Mass : Integer := 0;
         begin
            for J in I..I+Length-2 loop
               Mass := Mass + Mass_Table(Peptide((J-1) mod Length + 1));
               Spectrum(Index) := Mass;
               Index := Index + 1;
            end loop;
         end;
      end loop;
      
      return Spectrum(1..Index-1);
   end Cyclic_Spectrum;
   
   -- Function to calculate score of a peptide against a spectrum
   function Score(Peptide : Peptide; Spectrum : Integer_Vector) return Integer is
      Cyclic_Spectrum : constant Integer_Vector := Cyclic_Spectrum(Peptide);
      Score : Integer := 0;
      Spectrum_Map : array (Integer range 0..3000) of Integer := (others => 0);
   begin
      -- Count occurrences in spectrum
      for I in Spectrum'Range loop
         Spectrum_Map(Spectrum(I)) := Spectrum_Map(Spectrum(I)) + 1;
      end loop;
      
      -- Count matches
      for I in Cyclic_Spectrum'Range loop
         if Spectrum_Map(Cyclic_Spectrum(I)) > 0 then
            Score := Score + 1;
            Spectrum_Map(Cyclic_Spectrum(I)) := Spectrum_Map(Cyclic_Spectrum(I)) - 1;
         end if;
      end loop;
      
      return Score;
   end Score;
   
   -- Function to expand leaderboard
   function Expand(Leaderboard : Peptide_Vector; Amino_Acids : Character) return Peptide_Vector is
      Result : Peptide_Vector(1..Leaderboard'Length * 20);
      Index : Integer := 1;
   begin
      for I in Leaderboard'Range loop
         for Amino in 'G'..'W' loop
            declare
               New_Peptide : Peptide(1..Leaderboard(I)'Length + 1);
            begin
               for J in Leaderboard(I)'Range loop
                  New_Peptide(J) := Leaderboard(I)(J);
               end loop;
               New_Peptide(Leaderboard(I)'Length + 1) := Amino;
               Result(Index) := New_Peptide;
               Index := Index + 1;
            end;
         end loop;
      end loop;
      
      return Result(1..Index-1);
   end Expand;
   
   -- Function to trim leaderboard
   function Trim(Leaderboard : Peptide_Vector; Spectrum : Integer_Vector; N : Integer) return Peptide_Vector is
      Scores : array (1..Leaderboard'Length) of Integer;
      Trimmed : Peptide_Vector(1..N);
      Index : Integer := 1;
   begin
      -- Calculate scores
      for I in Leaderboard'Range loop
         Scores(I) := Score(Leaderboard(I), Spectrum);
      end loop;
      
      -- Sort by score (descending)
      for I in 1..Leaderboard'Length loop
         for J in I+1..Leaderboard'Length loop
            if Scores(J) > Scores(I) then
               declare
                  Temp_Score : constant Integer := Scores(I);
                  Temp_Peptide : constant Peptide := Leaderboard(I);
               begin
                  Scores(I) := Scores(J);
                  Scores(J) := Temp_Score;
                  Leaderboard(I) := Leaderboard(J);
                  Leaderboard(J) := Temp_Peptide;
               end;
            end if;
         end loop;
      end loop;
      
      -- Take top N
      for I in 1..Integer'Min(N, Leaderboard'Length) loop
         Trimmed(I) := Leaderboard(I);
      end loop;
      
      return Trimmed(1..Integer'Min(N, Leaderboard'Length));
   end Trim;
   
   -- Main algorithm
   procedure Leaderboard_Cyclopeptide_Sequencing_Main(Spectrum : Integer_Vector) is
      Leaderboard : Peptide_Vector(1..1);
      Leaderboard_Size : Integer := 1;
      Best_Peptide : Peptide(1..0);
      Best_Score : Integer := 0;
      Current_Spectrum : constant Integer_Vector := Spectrum;
   begin
      -- Initialize leaderboard with empty peptide
      Leaderboard(1) := (1..0 => 'G');
      
      loop
         -- Expand leaderboard
         declare
            Expanded : Peptide_Vector(1..Leaderboard_Size * 20);
            Expanded_Size : Integer := 0;
         begin
            for I in 1..Leaderboard_Size loop
               for Amino in 'G'..'W' loop
                  declare
                     New_Peptide : Peptide(1..Leaderboard(I)'Length + 1);
                  begin
                     for J in Leaderboard(I)'Range loop
                        New_Peptide(J) := Leaderboard(I)(J);
                     end loop;
                     New_Peptide(Leaderboard(I)'Length + 1) := Amino;
                     Expanded_Size := Expanded_Size + 1;
                     Expanded(Expanded_Size) := New_Peptide;
                  end;
               end loop;
            end loop;
            
            -- Trim expanded leaderboard
            Leaderboard := Trim(Expanded, Current_Spectrum, N);
            Leaderboard_Size := Leaderboard'Length;
         end;
         
         -- Remove peptides from leaderboard that cannot be extended
         declare
            Trimmed : Peptide_Vector(1..Leaderboard_Size);
            Trimmed_Size : Integer := 0;
         begin
            for I in 1..Leaderboard_Size loop
               declare
                  Peptide : constant Peptide := Leaderboard(I);
                  Peptide_Spectrum : constant Integer_Vector := Linear_Spectrum(Peptide);
                  Score : constant Integer := Score(Peptide, Current_Spectrum);
               begin
                  if Score > Best_Score then
                     Best_Score := Score;
                     Best_Peptide := Peptide;
                  end if;
                  
                  -- Keep peptides with good scores
                  if Score >= Best_Score then
                     Trimmed_Size := Trimmed_Size + 1;
                     Trimmed(Trimmed_Size) := Peptide;
                  end if;
               end;
            end loop;
            
            Leaderboard := Trimmed(1..Trimmed_Size);
            Leaderboard_Size := Trimmed_Size;
         end;
         
         -- Check if we found a complete solution
         if Leaderboard_Size = 0 then
            exit;
         end if;
      end loop;
      
      -- Output result
      Put_Line("Best peptide:");
      for I in Best_Peptide'Range loop
         Put(Best_Peptide(I));
      end loop;
      New_Line;
   end Leaderboard_Cyclopeptide_Sequencing_Main;
   
begin
   -- Example usage:
   -- Sample spectrum for testing
   declare
      Sample_Spectrum : constant Integer_Vector := (0, 71, 113, 129, 147, 203, 221, 237, 253, 307, 323, 341, 359, 377, 403, 421, 437, 453, 471, 489, 507, 525, 543, 561, 579, 597, 615, 633, 651, 669, 687, 705, 723, 741, 759, 777, 795, 813, 831, 849, 867, 885, 903, 921, 939, 957, 975, 993, 1011, 1029, 1047, 1065, 1083, 1101, 1119, 1137, 1155, 1173, 1191, 1209, 1227, 1245, 1263, 1281, 1299, 1317, 1335, 1353, 1371, 1389, 1407, 1425, 1443, 1461, 1479, 1497, 1515, 1533, 1551, 1569, 1587, 1605, 1623, 1641, 1659, 1677, 1695, 1713, 1731, 1749, 1767, 1785, 1803, 1821, 1839, 1857, 1875, 1893, 1911, 1929, 1947, 1965, 1983, 2001, 2019, 2037, 2055, 2073, 2091, 2109, 2127, 2145, 2163, 2181, 2199, 2217, 2235, 2253, 2271, 2289, 2307, 2325, 2343, 2361, 2379, 2397, 2415, 2433, 2451, 2469, 2487, 2505, 2523, 2541, 2559, 2577, 2595, 2613, 2631, 2649, 2667, 2685, 2703, 2721, 2739, 2757, 2775, 2793, 2811, 2829, 2847, 2865, 2883, 2901, 2919, 2937, 2955, 2973, 2991, 3009, 3027, 3045, 3063, 3081, 3099, 3117, 3135, 3153, 3171, 3189, 3207, 3225, 3243, 3261, 3279, 3297, 3315, 3333, 3351, 3369, 3387, 3405, 3423, 3441, 3459, 3477, 3495, 3513, 3531, 3549, 3567, 3585, 3603, 3621, 3639, 3657, 3675, 3693, 3711, 3729, 3747, 3765, 3783, 3801, 3819, 3837, 3855, 3873, 3891, 3909, 3927, 3945, 3963, 3981, 3999, 4017, 4035, 4053, 4071, 4089, 4107, 4125, 4143, 4161, 4179, 4197, 4215, 4233, 4251, 4269, 4287, 4305, 4323, 4341, 4359, 4377, 4395, 4413, 4431, 4449, 4467, 4485, 4503, 4521, 4539, 4557, 4575, 4593, 4611, 4629, 4647, 4665, 4683, 4701, 4719, 4737, 4755, 4773, 4791, 4809, 4827, 4845, 4863, 4881, 4899, 4917, 4935, 4953, 4971, 4989, 5007, 5025, 5043, 5061, 5079, 5097, 5115, 5133, 5151, 5169, 5187, 5205, 5223, 5241, 5259, 5277, 5295, 5313, 5331, 5349, 5367, 5385, 5403, 5421, 5439, 5457, 5475, 5493, 5511, 5529, 5547, 5565, 5583, 5601, 5619, 5637, 5655, 5673, 5691, 5709, 5727, 5745, 5763, 5781, 5799, 5817, 5835, 5853, 5871, 5889, 5907, 5925, 5943, 5961, 5979, 5997, 6015, 6033, 6051, 6069, 6087, 6105, 6123, 6141, 6159, 6177, 6195, 6213, 6231, 6249, 6267, 6285, 6303, 6321, 6339, 6357, 6375, 6393, 6411, 6429, 6447, 6465, 6483, 6501, 6519, 6537, 6555, 6573, 6591, 6609, 6627, 6645, 6663, 6681, 6699, 6717, 6735, 6753, 6771, 6789, 6807,