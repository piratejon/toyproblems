
drop table if exists tmp_02
;

create table tmp_02 (input text)
;

\copy tmp_02 from './input'
;

select
  sum((2 * (a*b + a*c + c*b)) + least(a*b, a*c, c*b)) part_1
  , sum((a*b*c) + (2*least(a+b, a+c, c+b))) part_2
from (
  select
    cast(a[1] as int) a
    , cast(a[2] as int) b
    , cast(a[3] as int) c
    , x
  from (
      select
        string_to_array(input, 'x') a
        , row_number() over () x
      from tmp_02
    ) x
) x
;

