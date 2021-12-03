with Ada.Text_IO; use Ada.Text_IO;

with P02;

procedure Main is
  filename : String := "input.txt";
begin
  P02.Part1(filename);
  P02.Part2(filename);
end Main;
