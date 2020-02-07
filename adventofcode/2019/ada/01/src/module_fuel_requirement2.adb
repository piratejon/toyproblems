with Ada.Numerics;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Module_Fuel_Requirement;

function Module_Fuel_Requirement2 (Mass : Integer) return Long_Integer is
  M : Long_Integer := 0;
  T : Integer := Mass;
begin
  loop
    T := Module_Fuel_Requirement(T);
    exit when T <= 0;
    M := M + Long_Integer(T);
  end loop;
  return M;
end;
