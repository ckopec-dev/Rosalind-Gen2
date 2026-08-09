```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Implement_ChromosomeToCycle is
   
   type Chromosome is array (Positive range <>) of Integer;
   type Cycle is array (Positive range <>) of Integer;
   
   function Chromosome_To_Cycle(P : Chromosome) return Cycle is
      Result : Cycle(1..2*P'Length);
      Index  : Positive := 1;
   begin
      for I in P'Range loop
         if P(I) > 0 then
            Result(Index)     := 2*P(I) - 1;
            Result(Index+1)   := 2*P(I);
         else
            Result(Index)     := -2*P(I);
            Result(Index+1)   := -2*P(I) - 1;
         end if;
         Index := Index + 2;
      end loop;
      
      return Result;
   end Chromosome_To_Cycle;
   
   -- Test function
   procedure Test_Chromosome_To_Cycle is
      Test_Chromosome : constant Chromosome := (1, -2, 3);
      Cycle_Result    : Cycle;
   begin
      Cycle_Result := Chromosome_To_Cycle(Test_Chromosome);
      
      Put("Input chromosome: ");
      for I in Test_Chromosome'Range loop
         Put(Test_Chromosome(I), Width => 4);
      end loop;
      New_Line;
      
      Put("Output cycle:     ");
      for I in Cycle_Result'Range loop
         Put(Cycle_Result(I), Width => 4);
      end loop;
      New_Line;
   end Test_Chromosome_To_Cycle;
   
begin
   Test_Chromosome_To_Cycle;
end Implement_ChromosomeToCycle;
```