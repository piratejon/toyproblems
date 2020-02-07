with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Module_Fuel_Requirement2;

-- input is a list of module masses, one per line

procedure Part2 is
  -- Part1 wants to find the sum of all individual module masses in the input
  F         : File_Type;
  File_Name : constant String := "input";
  Input_Mass : Ada.Strings.Unbounded.Unbounded_String;
  Integer_Mass : Integer;
  Fuel : Long_Integer := 0;
begin
  Open (F, In_File, File_Name);
  while not End_Of_File (F) loop
    Input_Mass := To_Unbounded_String(Get_Line (F));
    Integer_Mass := Integer'Value(To_String(Input_Mass));
    Fuel := Fuel + Module_Fuel_Requirement2(Integer_Mass);
  end loop;
  Close (F);
  Put(Long_Integer'Image(Fuel));
end Part2;
