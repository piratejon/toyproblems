with Ada.Text_IO;
package body P02 is

  function ParseLine(line : String; length : Natural) return StepType is
    r : StepType;
  begin
    r.Direction := P02.Invalid;
    r.Count := 0;
    for i in line'Range loop
      if line(i) = ' ' then
        -- Ada.Text_IO.Put_Line("line: '" & line & "', i: " & i'Image & ", line'First: " & line'First'Image & ", line'Last: " & line'Last'Image);
        if line(line'First .. line'First + i - 2) = "up" then
            r.Direction := Up;
        elsif line(line'First .. line'First + i - 2) = "down" then
            r.Direction := Down;
        elsif line(line'First .. line'First + i - 2) = "forward" then
            r.Direction := Forward;
        else
          Ada.Text_IO.Put_Line("could not parse '" & line(line'First .. line'First + i - 2) & "'");
        end if;

        r.Count := Integer'Value(line(line'First + i .. line'First + length - 1));

        -- Ada.Text_IO.Put_Line("number portion: '" & line(line'First + i .. line'First + length - 1) & "' read as " & r.Count'Image);

        exit;
      end if;
    end loop;
    return r;
  end ParseLine;

  procedure LineByLinePart1(filename : String) is
    HPos : Integer := 0;
    Depth : Integer := 0;
    Depth2 : Integer := 0;
    Aim : Integer := 0;
    Result : Integer;
    Result2 : Integer;
    Last : Natural;
    CurrentStep : StepType;
    F : Ada.Text_IO.File_Type;
    Line : String (1..80);
  begin
    Ada.Text_IO.Open (F, Ada.Text_IO.In_File, filename);
    while not Ada.Text_IO.End_Of_File(F) loop
      Ada.Text_IO.Get_Line(F, Line, Last);
      CurrentStep := P02.ParseLine(Line, Last);
      case CurrentStep.Direction is
        when Forward =>
          Ada.Text_IO.Put_Line("incrementing hpos: " & HPos'Image & "; Depth2: " & Depth2'Image);
          HPos := HPos + CurrentStep.Count;
          Depth2 := Depth2 + (Aim * CurrentStep.Count);

        when Up =>
          Depth := Depth - CurrentStep.Count;
          -- Depth2 := Depth2 - CurrentStep.Count;
          Aim := Aim - CurrentStep.Count;

        when Down =>
          Depth := Depth + CurrentStep.Count;
          -- Depth2 := Depth2 + CurrentStep.Count;
          Aim := Aim + CurrentStep.Count;

        when Invalid =>
          Ada.Text_IO.Put_Line("Could not parse '" & Line & "'");
      end case;
    end loop;
    Ada.Text_IO.Close(F);
    Result := HPos * Depth;
    Result2 := HPos * Depth2;
    Ada.Text_IO.Put_Line("HPos: " & HPos'Image & "; Depth: " & Depth'Image & "; Result: " & Result'Image & "; Aim: " & Aim'Image & "; Depth2: " & Depth2'Image & "; Result2: " & Result2'Image);
  end LineByLinePart1;

  procedure Part1(filename : String) is
  begin
    Ada.Text_IO.Put_Line(filename);
    P02.LineByLinePart1(filename);
  end Part1;

  procedure Part2(filename : String) is
  begin
    Ada.Text_IO.Put_Line(filename);
    -- P01.LineByLinePart2(filename);
  end Part2;

end P02;
