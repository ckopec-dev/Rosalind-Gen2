# Rosalind Problem: Compute the Score of a Cyclic Peptide Against a Spectrum

## Problem Understanding

We need to compute the score of a cyclic peptide against a given spectrum. The score is the number of matching theoretical spectrum peaks with the experimental spectrum peaks.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Compute_Score_Of_Cyclic_Peptide_Against_Spectrum is
   
   -- Define the amino acid masses
   type Amino_Acid is (A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y);
   type Mass_Array is array (Amino_Acid) of Integer;
   
   -- Mass values for each amino acid
   Masses : constant Mass_Array := (
      A => 71, C => 103, D => 115, E => 129, F => 147,
      G => 57,  H => 137, I => 113, K => 128, L => 113,
      M => 131, N => 114, P => 97,  Q => 128, R => 156,
      S => 87,  T => 101, V => 99,  W => 186, Y => 163
   );
   
   -- Function to get mass of an amino acid
   function Get_Mass(AA : Amino_Acid) return Integer is
   begin
      return Masses(AA);
   end Get_Mass;
   
   -- Convert peptide string to array of amino acids
   function String_To_Peptide(Peptide : String) return Vector is
      Result : Vector;
      I : Integer := 1;
   begin
      while I <= Peptide'Length loop
         case Peptide(I) is
            when 'A' => Append(Result, A);
            when 'C' => Append(Result, C);
            when 'D' => Append(Result, D);
            when 'E' => Append(Result, E);
            when 'F' => Append(Result, F);
            when 'G' => Append(Result, G);
            when 'H' => Append(Result, H);
            when 'I' => Append(Result, I);
            when 'K' => Append(Result, K);
            when 'L' => Append(Result, L);
            when 'M' => Append(Result, M);
            when 'N' => Append(Result, N);
            when 'P' => Append(Result, P);
            when 'Q' => Append(Result, Q);
            when 'R' => Append(Result, R);
            when 'S' => Append(Result, S);
            when 'T' => Append(Result, T);
            when 'V' => Append(Result, V);
            when 'W' => Append(Result, W);
            when 'Y' => Append(Result, Y);
         end case;
         I := I + 1;
      end loop;
      return Result;
   end String_To_Peptide;
   
   -- Compute theoretical spectrum for a cyclic peptide
   function Compute_Theoretical_Spectrum(Peptide : Vector) return Vector is
      Spectrum : Vector;
      N : constant Integer := Length(Peptide);
   begin
      -- Add 0 mass (empty prefix)
      Append(Spectrum, 0);
      
      -- For each possible subpeptide (cyclic)
      for Start in 1 .. N loop
         declare
            Mass : Integer := 0;
         begin
            for I in 0 .. N - 1 loop
               Mass := Mass + Get_Mass(Element(Peptide, (Start + I - 1) mod N + 1));
               Append(Spectrum, Mass);
            end loop;
         end;
      end loop;
      
      return Spectrum;
   end Compute_Theoretical_Spectrum;
   
   -- Count how many peaks in Spectrum match peaks in Experimental_Spectrum
   function Score_Peptide_With_Spectrum(Peptide : Vector; Experimental_Spectrum : Vector) return Integer is
      Theoretical_Spectrum : Vector := Compute_Theoretical_Spectrum(Peptide);
      Score : Integer := 0;
      I, J : Integer;
   begin
      -- Sort both spectra for easier comparison
      -- Note: In practice, we would sort, but for this problem we'll use a simpler approach
      for I in 1 .. Length(Theoretical_Spectrum) loop
         declare
            Peak : constant Integer := Element(Theoretical_Spectrum, I);
         begin
            for J in 1 .. Length(Experimental_Spectrum) loop
               if Element(Experimental_Spectrum, J) = Peak then
                  Score := Score + 1;
                  exit; -- Each peak in experimental spectrum matches at most once
               end if;
            end loop;
         end;
      end loop;
      
      return Score;
   end Score_Peptide_With_Spectrum;
   
   -- Simple vector implementation for demonstration
   type Vector is tagged record
      Data : array (1 .. 1000) of Integer;
      Length : Integer := 0;
   end record;
   
   procedure Append(V : in out Vector; Item : Integer) is
   begin
      V.Length := V.Length + 1;
      V.Data(V.Length) := Item;
   end Append;
   
   function Element(V : Vector; Index : Integer) return Integer is
   begin
      return V.Data(Index);
   end Element;
   
   function Length(V : Vector) return Integer is
   begin
      return V.Length;
   end Length;
   
   -- Main execution
   procedure Main is
      Peptide_String : constant String := "ISQ";
      Experimental_Spectrum_String : constant String := "0 71 113 129 147 203 221 237 260 278 306 324 341 358 376 404 421 439 467 485 503 521 539 557 575 593 611 629 647 665 683 701 719 737 755 773 791 809 827 845 863 881 900 918 936 954 972 990 1008 1026 1044 1062 1080 1098 1116 1134 1152 1170 1188 1206 1224 1242 1260 1278 1296 1314 1332 1350 1368 1386 1404 1422 1440 1458 1476 1494 1512 1530 1548 1566 1584 1602 1620 1638 1656 1674 1692 1710 1728 1746 1764 1782 1800 1818 1836 1854 1872 1890 1908 1926 1944 1962 1980 1998 2016 2034 2052 2070 2088 2106 2124 2142 2160 2178 2196 2214 2232 2250 2268 2286 2304 2322 2340 2358 2376 2394 2412 2430 2448 2466 2484 2502 2520 2538 2556 2574 2592 2610 2628 2646 2664 2682 2700 2718 2736 2754 2772 2790 2808 2826 2844 2862 2880 2898 2916 2934 2952 2970 2988 3006 3024 3042 3060 3078 3096 3114 3132 3150 3168 3186 3204 3222 3240 3258 3276 3294 3312 3330 3348 3366 3384 3402 3420 3438 3456 3474 3492 3510 3528 3546 3564 3582 3600 3618 3636 3654 3672 3690 3708 3726 3744 3762 3780 3798 3816 3834 3852 3870 3888 3906 3924 3942 3960 3978 3996 4014 4032 4050 4068 4086 4104 4122 4140 4158 4176 4194 4212 4230 4248 4266 4284 4302 4320 4338 4356 4374 4392 4410 4428 4446 4464 4482 4500 4518 4536 4554 4572 4590 4608 4626 4644 4662 4680 4698 4716 4734 4752 4770 4788 4806 4824 4842 4860 4878 4896 4914 4932 4950 4968 4986 5004 5022 5040 5058 5076 5094 5112 5130 5148 5166 5184 5202 5220 5238 5256 5274 5292 5310 5328 5346 5364 5382 5400 5418 5436 5454 5472 5490 5508 5526 5544 5562 5580 5598 5616 5634 5652 5670 5688 5706 5724 5742 5760 5778 5796 5814 5832 5850 5868 5886 5904 5922 5940 5958 5976 5994 6012 6030 6048 6066 6084 6102 6120 6138 6156 6174 6192 6210 6228 6246 6264 6282 6300 6318 6336 6354 6372 6390 6408 6426 6444 6462 6480 6498 6516 6534 6552 6570 6588 6606 6624 6642 6660 6678 6696 6714 6732 6750 6768 6786 6804 6822 6840 6858 6876 6894 6912 6930 6948 6966 6984 7002 7020 7038 7056 7074 7092 7110 7128 7146 7164 7182 7200 7218 7236 7254 7272 7290 7308 7326 7344 7362 7380 7398 7416 7434 7452 7470 7488 7506 7524 7542 7560 7578 7596 7614 7632 7650 7668 7686 7704 7722 7740 7758 7776 7794 7812 7830 7848 7866 7884 7902 7920 7938 7956 7974 7992 8010 8028 8046 8064 8082 8100 8118 8136 8154 8172 8190 8208 8226 8244 8262 8280 8298 8316 8334 8352 8370 8388 8406 8424 8442 8460 8478 8496 8514 8532 8550 8568 8586 8604 8622 8640 8658 8676 8694 8712 8730 8748 8766 8784 8802 8820 8838 8856 8874 8892 8910 8928 8946 8964 8982 9000 9018 9036 9054 9072 9090 9108 9126 9144 9162 9180 9198 9216 9234 9252 9270 9288 9306 9324 9342 9360 9378 9396 9414 9432 9450 9468 9486 9504 9522 9540 9558 9576 9594 9612 9630 9648 9666 9684 9702 9720 9738 9756 9774 9792 9810 9828 9846 9864 9882 9900 9918 9936 9954 9972 9990 10008 10026 10044 10062 10080 10098 10116 10134 10152 10170 10188 10206 10224 10242 10260 10278 10296 10314 10332 10350 10368 10386 10404 10422 10440 10458 10476 10494 10512 10530 10548 10566 10584 10602 10620 10638 10656 10674 10692 10710 10728 10746 10764 10782 10800 10818 10836 10854 10872 10890 10908 10926 10944 10962 10980 10998 11016 11034 1