package P02 is
  type DirectionType is (
    Invalid,
    Forward,
    Down,
    Up
  );

  type StepType is record
    Direction : DirectionType;
    Count : Integer;
  end record;

  function ParseLine(line : String; length : Natural) return StepType;

  procedure Part1(filename : String);
  procedure Part2(filename : String);

end P02;
