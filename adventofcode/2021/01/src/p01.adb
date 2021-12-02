with Ada.Text_IO;
package body P01 is

  procedure LineByLinePart1(filename : String) is
    F : Ada.Text_IO.File_Type;
    Current_Integer : Integer;
    Last_Integer : Integer;
    Increase_Count : Integer := 0;
  begin
    Ada.Text_IO.Open (F, Ada.Text_IO.In_File, filename);
    Last_Integer := Integer'Value(Ada.Text_IO.Get_Line(F));
    while not Ada.Text_IO.End_Of_File(F) loop
      Current_Integer := Integer'Value(Ada.Text_IO.Get_Line(F));
      If Current_Integer > Last_Integer Then
        Increase_Count := Increase_Count + 1;
      End If;
      Last_Integer := Current_Integer;
    end loop;
    Ada.Text_IO.Close(F);
    Ada.Text_IO.Put_Line("The integers increased " & Increase_Count'Image & " Times!");
  end LineByLinePart1;

  procedure LineByLinePart2(filename : String) is
    F : Ada.Text_IO.File_Type;
    A : Integer;
    B : Integer;
    C : Integer;
    D : Integer;
    Increased : Integer := 0;
  begin
    Ada.Text_IO.Open (F, Ada.Text_IO.In_File, filename);
    A := Integer'Value(Ada.Text_IO.Get_Line(F));
    B := Integer'Value(Ada.Text_IO.Get_Line(F));
    C := Integer'Value(Ada.Text_IO.Get_Line(F));
    while not Ada.Text_IO.End_Of_File(F) loop
      D := Integer'Value(Ada.Text_IO.Get_Line(F));
      if (A + B + C) < (B + C + D) then
        Increased := Increased + 1;
      end if;

      A := B;
      B := C;
      C := D;
    end loop;
    Ada.Text_IO.Close(F);
    Ada.Text_IO.Put_Line("The integers increased " & Increased'Image & " Times!");
  end LineByLinePart2;

  procedure Part1(filename : String) is
  begin
    Ada.Text_IO.Put_Line(filename);
    P01.LineByLinePart1(filename);
  end Part1;

  procedure Part2(filename : String) is
  begin
    Ada.Text_IO.Put_Line(filename);
    P01.LineByLinePart2(filename);
  end Part2;

end P01;
