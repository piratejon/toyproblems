
with Ada.Numerics;
-- module fuel requirement is found by:
-- * divide mass by 3
-- * round down
-- * subtract 2
-- Examples:
-- * m=12, (m / 3) - 2 = 2
-- * m=14, (m / 3) - 2 = 2
-- * m=1969, (m / 3) - 2 = 654
-- * m=100756, (m / 3) - 2 = 33583

function Module_Fuel_Requirement (Mass : Integer) return Integer is
begin
  return (Mass / 3) - 2;
end;
